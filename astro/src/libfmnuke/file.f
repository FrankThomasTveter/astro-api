#__file: 'anahdr.F' 0100664    **DO NOT DELETE**
      SUBROUTINE ANAHDR(MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &        CODHDR,PPDAT,MAXGRP,NRGRP,HDRGRP,
     &        IRC)
C
      IMPLICIT NONE
C
      INTEGER NROPT
      PARAMETER(NROPT=5)
C
      INTEGER MAXHDR, NRHDR
      CHARACTER*250 HDR250(MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR), CODHDR(MAXHDR)
      LOGICAL PPDAT(NROPT,MAXHDR)
      INTEGER MAXGRP, HDRGRP(6,MAXGRP),NRGRP
      INTEGER IRC
C
      INTEGER LENH,LENGTH,II,JJ,POS(2),LEV,MINGRP
      EXTERNAL LENGTH
      CHARACTER*250 BUFF250
      CHARACTER*1 DEL(2)
      CHARACTER*8 MYNAME
      DATA MYNAME/'ANAHDR'/
C
C     FIND LENGTH OF HEADER, AND LOCATION OF []...
C
      MINGRP=-MAXHDR
      NRGRP=0
      LENH=1
      II=1
      DO WHILE(II.LE.MAXHDR)
         CALL CHOP0(HDR250(II),250)
         LENH=LENGTH(HDR250(II),250,LENH)
         IF (LENH.EQ.0) THEN
C     THE HEADER IS BLANK
            ACTHDR(II)=.FALSE.
         ELSE
            ACTHDR(II)=.TRUE.
            NRHDR=II
            LEV=0
            DEL(1)='['
            DEL(2)=']'
            POS(1)=1
            POS(2)=250
            CALL ITEM(HDR250(II),250,DEL,LEV,POS,IRC)
            IF (IRC.NE.0 .OR. LEV .EQ. -1) THEN
               WRITE(*,*) MYNAME,
     &              'Error: missing [] in header, ',II,'.'
               IF(IRC.EQ.0) IRC=250
               RETURN
            ENDIF
            NRLEN(1,II)=POS(1)-1
            NRLEN(3,II)=NRLEN(1,II)
            NRLEN(2,II)=0
            do jj=nrlen(3,ii),1,-1
               if (HDR250(II)(jj:jj).EQ.'-') nrlen(2,ii)=jj
            end do
            if (nrlen(2,ii).ne.0) then
               nrlen(1,ii)=nrlen(2,ii)-1
               nrlen(2,ii)=nrlen(2,ii)+1
            else 
               nrlen(2,ii)=nrlen(3,ii)
            end if
            IF (NRLEN(1,II).NE.0) THEN
C     search for "-", which defines short header term
               IF (HDR250(II)(NRLEN(1,II):NRLEN(1,II)).EQ.' ')
     &              NRLEN(1,II)=NRLEN(1,II)-1
            ELSE
               WRITE(*,*) MYNAME,'Error: header too short, ',II,'.'
               IF(IRC.EQ.0) IRC=101
            ENDIF
C
C     GET LENGTH OF DATA-BODY
C
            IF (HDR250(II)(POS(1)+1:POS(2)-1).EQ.'*') THEN
               CODHDR(II)=-1
            ELSE
               READ(HDR250(II)(POS(1)+1:POS(2)-1),*,IOSTAT=IRC) 
     &              CODHDR(II)
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Unable to read length of'//
     &                 ' data body for header ',II,'.'
                  RETURN
               END IF
            END IF
C
            BUFF250=HDR250(II)(POS(2)+1:LENH)
            CALL CHOP0(BUFF250,250)
C
C     CHECK FOR "VFMLR" FLAGS
C
            CALL GETFLAGS(BUFF250,
     &           PPDAT(1,II),
     &           IRC)
            IF (IRC.NE.0) THEN
               WRITE(*,*) MYNAME,'Error return from GETFLAGS.',IRC
               RETURN
            END IF
C
C     FIND HEADER GROUPS
C
            CALL GETGRP(BUFF250,II,
     &           MAXGRP,MINGRP,NRGRP,HDRGRP,
     &           IRC)
            IF (IRC.NE.0) THEN
               WRITE(*,*) MYNAME,'Error return from GETGRP.',IRC
               RETURN
            END IF
         END IF
         II=II+1
      END DO
C     
      END

#__file: 'analfl.F' 0100664    **DO NOT DELETE**
      subroutine ANALFL(MAXHDR,NRHDR,HDR250,
     &     ACTHDR,LFLDAT,NRLEN,
     &     MAXGRP,NRGRP,HDRGRP,
     &     IRC)
C
      IMPLICIT NONE
C
      INTEGER NROPT
      PARAMETER(NROPT=5)
C
      INTEGER MAXHDR, NRHDR
      CHARACTER*250 HDR250(MAXHDR),BHDR250(MAXHDR),BBHDR250
      LOGICAL ACTHDR(MAXHDR),LFLDAT(MAXHDR),BLFLDAT(MAXHDR),BBLFLDAT
      INTEGER NRLEN(3,MAXHDR),BNRLEN(MAXHDR),BBNRLEN
      INTEGER MAXGRP,NRGRP,
C     ! 1=parent, 2=next sibling, 3=first child,  4=type, 5=id/chdr, 6=lvl
     &     HDRGRP(6,MAXGRP)
      INTEGER IRC
C
      INTEGER MAXGROUPS,NRGROUPS
      PARAMETER (MAXGROUPS=250)
      INTEGER DATAGRP(10,MAXGROUPS)
      INTEGER LVLGRP(MAXGROUPS)
      INTEGER MAXIND, NRIND
      PARAMETER(MAXIND=300)
      INTEGER INDX(10,MAXIND)
C
      integer lvl,mxlvl,nrc,tot,cgrp,pos
      logical lflgrp(maxgroups)
C
      INTEGER II, JJ, KK, LL, MM, CNT,ST,EN,ENHDR
      INTEGER INRHDR,IND
      LOGICAL LAND,BDONE,LSET,FOUND,OK,QLFL
      EXTERNAL QLFL
      CHARACTER*16 MYNAME
      DATA MYNAME /'ANALFL'/
C
      character*50 cerr50
      integer lenc, length,CERR
      external length
      logical error
C
      IF (NRHDR.GT.MAXHDR) THEN
         WRITE(*,*) MYNAME,'Nrhdr exceeds Maxhdr:',NRHDR,MAXHDR
         IRC=991
         RETURN
      END IF
C
C     find mxlvl
C
      mxlvl=-1
      do ii=1,nrgrp
         mxlvl=max(mxlvl,HDRGRP(6,ii))
         if (HDRGRP(3,ii).eq.0.and.HDRGRP(4,ii).ne.-1) then ! un-terminated chain
            write(*,*) myname,'System error: no termination for:',ii
            irc=911
            return
                  END IF
               END DO
C     
C     SET GROUP FLAGS
C
      do lvl=mxlvl,1,-1
         ! write(*,*) 'ANALFL LVL:',lvl,mxlvl
         do ii=nrgrp,1,-1

            ! write(*,*) 'ANALFL GRP:',ii,nrgrp

            IF (HDRGRP(6,II).EQ.LVL) THEN
               NRC=0
               TOT=0
               ERROR=.FALSE.
               CERR50=''
               IF (HDRGRP(4,II).EQ.-1) THEN ! this is a header
                  LFLGRP(II)=LFLDAT(-HDRGRP(5,II))
               ELSE             ! loop through children
                  IND=HDRGRP(3,II)
                  BDONE=(IND.EQ.0)
                  DO WHILE (.NOT. BDONE)
                     IF (HDRGRP(4,II).EQ.1) THEN ! "&", ALL MUST BE PRESENT...
                        IF (HDRGRP(4,IND).NE.3) THEN ! ...EXCEPT THOSE OF TYPE "$"
                           TOT=TOT+1
                           IF (LFLGRP(IND)) NRC=NRC+1
                        ELSE
                           IF (LFLGRP(IND)) THEN ! record data if set
                              TOT=TOT+1
                              NRC=NRC+1
                           END IF
                        END IF
                        IF (LVL.EQ.1) THEN ! top level, missing & causes error
                           IF (.NOT.ERROR) THEN
                              ERROR=(TOT.NE.NRC)
                              IF (ERROR) CERR=IND
                           END IF
                        END IF
                     ELSE
                        TOT=TOT+1
                        IF (LFLGRP(IND))  NRC=NRC+1
                     END IF
                     IND=HDRGRP(2,IND)
                     BDONE=(IND.EQ.0)
                  END DO
C     
                  IF (LVL.EQ.1) THEN ! top level
                     CERR50 = 'MISSING INFORMATION'
                  ELSE IF (HDRGRP(4,II).EQ.1) THEN ! "&", ALL MUST BE PRESENT
                     ERROR      = (NRC.NE.0 .AND. NRC.NE.TOT) ! REPORT ERROR
                     CERR50 = 'MISSING INFORMATION'
                     LFLGRP(II) = (TOT.EQ.NRC .AND. TOT.NE.0)
                     IF (ERROR) CERR=II
                  ELSE IF (HDRGRP(4,II).EQ.2) THEN !"%", 0 OR 1 MUST BE PRESENT
                     ERROR=(NRC.GT.1) ! REPORT ERROR
                     CERR50 = 'SURPLUS INFORMATION'
                     LFLGRP(II)=(NRC.EQ.1)
                     IF (ERROR) CERR=II
                  ELSE IF (HDRGRP(4,II).EQ.3) THEN ! "$", 1 OR MORE BE PRESENT
                     LFLGRP(II)=(NRC.GE.1)
                  ELSE IF (HDRGRP(4,II).EQ.10) THEN ! raw header
                     LFLGRP(II)=(NRC.GE.1)
                  ELSE
                     WRITE(*,*) MYNAME,'Invalid type:',HDRGRP(4,II),ii
                     IRC=956
                     RETURN
                  END IF
C     
                  IF (ERROR) THEN ! report error and terminate
                     CALL REPERR (CERR,CERR50,MAXHDR,HDR250,
     &                    MAXGRP,NRGRP,HDRGRP,LFLGRP,
     &                    IRC)
                     IF (IRC.NE.0) THEN
                        WRITE(*,*) MYNAME,
     &                       'Error return from reperr.',IRC
                        RETURN
                     END IF
                     call chop0(cerr50,50)
                     lenc=length(cerr50,50,10)
                     write(*,*) myname,'Input error:',cerr50(1:lenc)
                     IRC=364
                     RETURN
                  END IF
               END IF
            END IF
         END DO
      END DO
C
C      II=-1
C      CERR50=''
C      CALL REPERR (II,CERR50,MAXHDR,BHDR250,MAXGRP,
C     &     MAXGROUPS,NRGROUPS,DATAGRP,LFLGRP,
C     &     MAXIND,NRIND,INDX,
C     &     IRC)
      return
      end
#__file: 'argument.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDARGUMENT
      IMPLICIT NONE
      LOGICAL ACTIVE,BDEB
      COMMON /CCARGUMENT/BDEB,ACTIVE
      DATA ACTIVE/.false./
      END BLOCK DATA


      SUBROUTINE ARGUMENT(STRING,N,CPOS,BUFF,QQ,RR)
C
C     FINDS UP TO TWO ARGUMENTS OF A FUNCTION
C
      IMPLICIT NONE
C
      INTEGER N
      CHARACTER*(*) STRING
C
      CHARACTER*250 BUFF,BUFF1,BUFF2,BUFF3,NUKEHEAD
C
      INTEGER LENB1,LENB2,LENB3,LENGTH
      EXTERNAL LENGTH,NUKEHEAD
      LOGICAL QQ(2),QQ1,QQ2,ISINT,ISREAL
      EXTERNAL ISINT,ISREAL
      INTEGER CPOS,POS2(2),CODE,
     &     RI1,RI2
      REAL RR(2),RR1,RR2
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'ARGUMENT'/
C
      LOGICAL ACTIVE,BDEB
      COMMON /CCARGUMENT/BDEB,ACTIVE
C
      BDEB=.false.
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.'
C
C     FIND NEXT OBJECT
C
      CODE=+1
      CALL OBJECT(STRING,N,CPOS,POS2,CODE)
      CALL RESET(BUFF1,250)
      CALL CEQUAL(BUFF,250,STRING,POS2(1),POS2(2))
      CALL CHOP0(BUFF,250)
      BUFF1=BUFF
      LENB1=LENGTH(BUFF1,250,LENB1)
C
C     INITIALISE
C
      QQ1=.FALSE.
      QQ2=.FALSE.
      RR1=0.0D0
      RR2=0.0D0
C
C     ANALYSE OBJECT
C
      IF (QQ(1)) THEN
C     FIND FIRST NUMERICAL ARGUMENT
         BUFF2=NUKEHEAD(BUFF1,250)
         BUFF3=BUFF1
         LENB2=LENGTH(BUFF2,250,LENB2)
         LENB3=LENGTH(BUFF3,250,LENB3)
         QQ1=ISREAL(BUFF2,RR1)
         IF (.NOT.QQ1) THEN
            QQ1=ISINT(BUFF2,RI1)
            IF (QQ1) RR1=REAL(RI1)
         ENDIF
C
         IF (BDEB) WRITE(*,*) MYNAME,'Debug:',QQ1,BUFF2,RR1
C
         IF (QQ1.AND.QQ(2)) THEN
C     FIND SECOND ARGUMENT
            LENB3=LENGTH(BUFF3,250,LENB3)
            QQ2=ISREAL(BUFF3,RR2)
            IF (.NOT.QQ2) THEN
               QQ2=ISINT(BUFF3,RI2)
               IF (QQ2) RR2=REAL(RI2)
            ENDIF
C
            IF (BDEB) WRITE(*,*) MYNAME,'Debug:',QQ2,BUFF3,RR2
C
         ELSEIF (QQ1) THEN
            QQ1=(LENB3.EQ.0)
         ENDIF
      ENDIF
C
C     ASSIGN NEW VALUES
C
      QQ(1)=QQ1
      QQ(2)=QQ2
      RR(1)=RR1
      RR(2)=RR2
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine ends.'
C
      RETURN
      END
#__file: 'atom.F' 0100664    **DO NOT DELETE**
      LOGICAL FUNCTION ATOM(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     CHECK IF STRING CONSISTS OF A SINGLE OBJECT                           *
C     ....(THAT DOES NOT NEED BRACKETS)                                     *
C     +                                                                     *
C     +                                                                     *
CI    STRING (C*N) = STRING TO BE EXAMINED                                  *
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
C
      INTEGER LENS,LENGTH,POS(2),CODE,START
      EXTERNAL LENGTH
      LOGICAL BUFF
C
      CALL CHOP0(STRING,N)
      LENS=LENGTH(STRING,N,1)
C
      START=1
      CODE=1
      CALL OBJECT(STRING,N,START,POS,CODE)
C
      BUFF=(POS(1).EQ.1 .AND. POS(2).EQ.LENS)
C
      ATOM=BUFF
C
      RETURN
      END
#__file: 'blobb.F' 0100664    **DO NOT DELETE**
      SUBROUTINE BLOBB
C
      IMPLICIT NONE
C
      CHARACTER*72 STR(29)
      INTEGER JJ
C
      STR(1)='                                   '//
     &     '                          ..         '
      STR(2)='                                  ,,'//
     &     ',                         MM .M     '
      STR(3)='                              ,!MMMM'//
     &     'MMM!,                     MM MM  ,. '
      STR(4)='      ., .M                .MMMMMMMM'//
     &     'MMMMMMMM.,          `MM.  MM MM .M` '
      STR(5)='    . M: M;  M          .MMMMMMMMMMM'//
     &     'MMMMMMMMMMM,          `MM,:M M`!M`  '
      STR(6)='   ;M MM M: .M        .MMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMM,         `MM`...`M    '
      STR(7)='    M;MM;M :MM      .MMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMM.       .MMMMMMMM    '
      STR(8)='    `M;M`M MM      MMMMMM  MMMMMMMMM'//
     &     'MMMMMMMM  MMMMMM.    ,,M.M.`MMM`    '
      STR(9)='     MM`MMMM      MMMMMM @@ MMMMMMMM'//
     &     'MMMMMMM @@ MMMMMMM.`M``MMMM;MM`     '
      STR(10)='    MM., ,MM     MMMMMMMM  MMMMMMMMM'//
     &     'MMMMMMMM  MMMMMMMMM      `.MMM      '
      STR(11)='    `MM;MMMMMMMM.MMMMMMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMMMMMM.      `MMM      '
      STR(12)='     ``.`MMM`  .MMMMMMMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMMMMMMM       MMMM     '
      STR(13)='      MMC      MMMMMMMMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMMMMMMM.      `MMMM    '
      STR(14)='     .MM      :MMMMMMMMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMMM``MMM       MMMMM   '
      STR(15)='     MMM      :M  `MMMMMMMMMMMMM.MMM'//
     &     'MM.MMMMMMMMMM`.MM  MM:M.    `MMMMM  '
      STR(16)='    .MMM   ...:M: :M.`MMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMM`.M``   MM:MMMMMMMMMMMM` '
      STR(17)='   AMMM..MMMMM:M.    :M.`MMMMMMMMMMM'//
     &     'MMMMMMMMM`.MM`     MM````````````   '
      STR(18)='   MMMMMMMMMMM:MM     `M`.M`MMMMMMMM'//
     &     'MMMMMM`.MC`M`     .MM               '
      STR(19)='    ``````````:MM.       `MM!M.`M-M-'//
     &     'M-M`M.`MM`        MMM               '
      STR(20)='               MMM.            `MMMM'//
     &     '!MMMM`            .MM               '
      STR(21)='                MMM.             ```'//
     &     '   ``            .MM`               '
      STR(22)='                 MMM.               '//
     &     '                MMM`                '
      STR(23)='                  MMMM            ,.'//
     &     'J.JJJJ.       .MMM`                 '
      STR(24)='                   MMMM.       `JJJJ'//
     &     'JJJ`JJJM   CMMMMM                   '
      STR(25)='                     MMMMM.    `JJJJ'//
     &     'JJJJ`JJJ .MMMMM`                    '
      STR(26)='                       MMMMMMMM.`  `'//
     &     'JJJJJ`JJMMMMM`                      '
      STR(27)='                         `MMMMMMMMM`'//
     &     'JJJJJ JJJJJ`                        '
      STR(28)='                            ``MMMMMM'//
     &     'JJJJJJJJJJ`                         '
      STR(29)='                                    '//
     &     '`JJJJJJJJ`                          '
C
C     WRITE "BLOBB" TO SCREEN
C
C      WRITE(*,*)
C      DO JJ=1,29
C         WRITE(*,*) STR(JJ)
C      ENDDO
C      WRITE(*,*)
C
      RETURN
      END
#__file: 'c2upper.F' 0100664    **DO NOT DELETE**
      SUBROUTINE C2Upper ( ChString )
      IMPLICIT NONE
c-----------------------------------------
c identification variable for "what"
      character*62 sccsid
c
      data sccsid
     &/'@(#) c2upper.F version 1.1 on 2/12/98'/
c
C
C Purpose:
C   Convert character string to upper case
C
C Method:
C   Loop through each character in ChString.
C   Convert any [a-z] to [A-Z]   (without assuming ASCII)
C
C Current Code Owner: R.J.Renshaw
C
C History:
C Version  Date      Comment
C
C  1.0      27/11/96  Original code. (R.J.Renshaw)
C
C Code Description:
C   FORTRAN 77, following AAPP standards
C
C Declarations:
C
C Global variables:
C
C Subroutine arguments
C   Scalar arguments with intent(InOut):
      CHARACTER*(*) ChString      ! character string
C
C Local Scalars:
      INTEGER       i             ! pointer that loops through ChString
      INTEGER       len_string    ! length of ChString
      CHARACTER*1   c             ! char in ChString pointed to by i
C
C- End of header
C
C     loop through ChString, converting lowercase characters
C
      len_string = LEN ( ChString )
      DO i = 1, len_string
C
C        Get the character pointed to by i
         c = ChString(i:i)
C
C        Is it a lower-case character ?   If so, convert to upper-case
         IF     ( c .EQ. "a" ) THEN
            ChString(i:i) = "A"
         ELSEIF ( c .EQ. "b" ) THEN
            ChString(i:i) = "B"
         ELSEIF ( c .EQ. "c" ) THEN
            ChString(i:i) = "C"
         ELSEIF ( c .EQ. "d" ) THEN
            ChString(i:i) = "D"
         ELSEIF ( c .EQ. "e" ) THEN
            ChString(i:i) = "E"
         ELSEIF ( c .EQ. "f" ) THEN
            ChString(i:i) = "F"
         ELSEIF ( c .EQ. "g" ) THEN
            ChString(i:i) = "G"
         ELSEIF ( c .EQ. "h" ) THEN
            ChString(i:i) = "H"
         ELSEIF ( c .EQ. "i" ) THEN
            ChString(i:i) = "I"
         ELSEIF ( c .EQ. "j" ) THEN
            ChString(i:i) = "J"
         ELSEIF ( c .EQ. "k" ) THEN
            ChString(i:i) = "K"
         ELSEIF ( c .EQ. "l" ) THEN
            ChString(i:i) = "L"
         ELSEIF ( c .EQ. "m" ) THEN
            ChString(i:i) = "M"
         ELSEIF ( c .EQ. "n" ) THEN
            ChString(i:i) = "N"
         ELSEIF ( c .EQ. "o" ) THEN
            ChString(i:i) = "O"
         ELSEIF ( c .EQ. "p" ) THEN
            ChString(i:i) = "P"
         ELSEIF ( c .EQ. "q" ) THEN
            ChString(i:i) = "Q"
         ELSEIF ( c .EQ. "r" ) THEN
            ChString(i:i) = "R"
         ELSEIF ( c .EQ. "s" ) THEN
            ChString(i:i) = "S"
         ELSEIF ( c .EQ. "t" ) THEN
            ChString(i:i) = "T"
         ELSEIF ( c .EQ. "u" ) THEN
            ChString(i:i) = "U"
         ELSEIF ( c .EQ. "v" ) THEN
            ChString(i:i) = "V"
         ELSEIF ( c .EQ. "w" ) THEN
            ChString(i:i) = "W"
         ELSEIF ( c .EQ. "x" ) THEN
            ChString(i:i) = "X"
         ELSEIF ( c .EQ. "y" ) THEN
            ChString(i:i) = "Y"
         ELSEIF ( c .EQ. "z" ) THEN
            ChString(i:i) = "Z"
         ENDIF  ! c
C
      ENDDO  ! i
C
      RETURN
      END
#__file: 'cequal.F' 0100664    **DO NOT DELETE**
      SUBROUTINE CEQUAL(STRING1,N,STRING2,POS1,POS2)
C     ***********************************************************************
C     +                                                                     *
C     SETS STRING1 EQUAL TO STRING2 (FROM POS1 TO POS2)                     *
C     +                                                                     *
CO    STRING1 (C*N1) = STRING TO BE OVERWRITTEN                             *
CI    N1 (I*4) = LENGTH OF STRING                                           *
CI    STRING2 (C*N2) = STRING FROM WHICH TO COPY DATA, WHERE N2y.GT.POS2     *
CI    POS1 (I*4) = START POSITION IN STRING2 FROM WHICH TO START COPYING    *
CI    POS2 (I*4) = END POSITION IN STRING2 FROM WHICH TO END COPYING        *
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
      INTEGER N,POS1,POS2
      CHARACTER*(*) STRING1, STRING2
      CHARACTER*1 C1
C
      INTEGER II,MAXX
C
      C1=CHAR(0)
      MAXX=MIN(N,POS2-POS1+1)
      DO II=1,MAXX
         STRING1(II:II)=STRING2(POS1+II-1:POS1+II-1)
      ENDDO
      MAXX=MAXX+1
      IF (MAXX.LT.N) THEN
         DO II=MAXX,N
            STRING1(II:II)=C1
         ENDDO
      ENDIF
C
      RETURN
      END
#__file: 'chop0.F' 0100664    **DO NOT DELETE**
      SUBROUTINE CHOP0(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES SURPLUS BLANKS FROM A STRING                                  *
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
      CHARACTER*1 C1
      INTEGER II,JJ
      LOGICAL LASTBL
C
      C1=CHAR(0)
      LASTBL=.TRUE.
      JJ=0
      II=0
      DO WHILE(II.LT.N)
         II=II+1
         IF (LASTBL .AND. STRING(II:II).EQ.C1) THEN
            II=N
         ELSE
            IF (STRING(II:II).NE.' ' .AND.
     &           STRING(II:II).NE.C1) THEN
C     ADDING CHARACTER TO BUFF
               JJ=JJ+1
               STRING(JJ:JJ)=STRING(II:II)
               LASTBL=.FALSE.
            ELSEIF (.NOT.LASTBL) THEN
C     ADDING (*ONE*) BLANK TO BUFF
               JJ=JJ+1
               STRING(JJ:JJ)=' '
               LASTBL=.TRUE.
            ENDIF
         ENDIF
      ENDDO
C
      IF (.NOT.LASTBL.OR.JJ.EQ.0) JJ=JJ+1
C
      DO II=JJ,N
         STRING(II:II)=C1
      ENDDO
      RETURN
      END
#__file: 'chopx.F' 0100664    **DO NOT DELETE**
      SUBROUTINE CHOPX(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES SURPLUS BLANKS FROM A STRING                                  *
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
      CHARACTER*1 C1
      INTEGER II,JJ
      LOGICAL LASTBL
C
      LASTBL=.TRUE.
      JJ=0
      II=0
      DO WHILE(II.LT.N)
         II=II+1
         IF (LASTBL .AND. ichar(STRING(II:II)).le.13) THEN
            II=N
         ELSE
            IF (STRING(II:II).NE.' ' .AND.
     &           ichar(STRING(II:II)).gt.13) THEN
C     ADDING CHARACTER TO BUFF
               JJ=JJ+1
               STRING(JJ:JJ)=STRING(II:II)
               LASTBL=.FALSE.
            ELSEIF (.NOT.LASTBL) THEN
C     ADDING (*ONE*) BLANK TO BUFF
               JJ=JJ+1
               STRING(JJ:JJ)=' '
               LASTBL=.TRUE.
            ENDIF
         ENDIF
      ENDDO
C
      IF (.NOT.LASTBL.OR.JJ.EQ.0) JJ=JJ+1
C
      DO II=JJ,N
         STRING(II:II)=char(0)
      ENDDO
      RETURN
      END
#__file: 'cleansome.F' 0100664    **DO NOT DELETE**
      SUBROUTINE CLEANSOME(STRING,N,POS)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES {}, AND REPLACES [] WITH () IN STRING                         *
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
      integer pos(2),startii,stopii
      CHARACTER*1 BUFF
C
      INTEGER II,LENGTH,LENS,DELL,IIZERO
      LOGICAL DIGIT,ACTIVE
      EXTERNAL LENGTH,DIGIT
C
      LENS=LENGTH(STRING,N,1)
      DELL=1
      startii=pos(1)
      stopii=pos(2)
      II=max(1,startii)
      DO WHILE(II.LE.min(stopii,LENS))
C     REPLACE [] WITH ()
         IF (STRING(II:II).EQ.'[') THEN
            BUFF='('
            CALL SUBSTR(STRING,N,BUFF,1,II,DELL)
         ELSEIF (STRING(II:II).EQ.']') THEN
            BUFF=')'
            CALL SUBSTR(STRING,N,BUFF,1,II,DELL)
         ENDIF
C     REMOVE 'PROTECTIVE BRACKETS' {}, OR GOTO NEXT CHARACTER
         IF (STRING(II:II).EQ.'{'.OR.STRING(II:II).EQ.'}') THEN
            CALL SUBSTR(STRING,N,BUFF,0,II,DELL)
            LENS=LENS-1
            stopii=stopii-1
         ELSE
            II=II+1
         ENDIF
      ENDDO
C
      LENS=LENGTH(STRING,N,LENS)
      CALL CHOP0(STRING,LENS)
C
      RETURN
      END
#__file: 'cleanup.F' 0100664    **DO NOT DELETE**
      SUBROUTINE CLEANUP(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES {}, AND REPLACES [] WITH () IN STRING                         *
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
      CHARACTER*1 BUFF
C
      INTEGER II,LENGTH,LENS,DELL,IIZERO
      LOGICAL DIGIT,ACTIVE
      EXTERNAL LENGTH,DIGIT
C
      LENS=LENGTH(STRING,N,1)
      DELL=1
      II=1
      DO WHILE(II.LE.LENS)
C     REPLACE [] WITH ()
         IF (STRING(II:II).EQ.'[') THEN
            BUFF='('
            CALL SUBSTR(STRING,N,BUFF,1,II,DELL)
         ELSEIF (STRING(II:II).EQ.']') THEN
            BUFF=')'
            CALL SUBSTR(STRING,N,BUFF,1,II,DELL)
         ENDIF
C     REMOVE 'PROTECTIVE BRACKETS' {}, OR GOTO NEXT CHARACTER
         IF (STRING(II:II).EQ.'{'.OR.STRING(II:II).EQ.'}') THEN
            CALL SUBSTR(STRING,N,BUFF,0,II,DELL)
            LENS=LENS-1
         ELSE
            II=II+1
         ENDIF
      ENDDO
C
      LENS=LENGTH(STRING,N,LENS)
      CALL CHOP0(STRING,LENS)
C
      RETURN
      END
#__file: 'debug.F' 0100664    **DO NOT DELETE**
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
#__file: 'defntnr.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDDEFNTNR
      IMPLICIT NONE
      INTEGER ICODE
      INTEGER JJ,
     &     POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE
      LOGICAL QREAL,OQREAL,OUT
      COMMON /CCDEFNTNR/ICODE,JJ,POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE,QREAL,OQREAL,OUT
      DATA ICODE /0/
      END BLOCK DATA


      LOGICAL FUNCTION DEFNTNR(CHAR,CODE,NEED)
C
C     RETURNS FALSE IF THE STRING WE ARE READING DEFINITELY IS NOT A NUMBER
C
C     CHAR (C*1) THE NEXT CHARACTER
C     CODE =+1 INITIALISE TO GO FORWARDS
C     CODE =-1 INITIALISE TO GO BACKWARDS
C     CODE =+1 CHECK CURRENT CHARACTER
C     NEED = .TRUE. IF WE NEED AT LEAST ONE MORE CHARACTER
C
      IMPLICIT NONE
C
      CHARACTER*1 CHAR
      INTEGER CODE
      LOGICAL NEED
C
      INTEGER ICODE
      CHARACTER*16 MYNAME
      DATA MYNAME /'DEFNTNR'/
C
      INTEGER JJ,
     &     POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE
      LOGICAL QREAL,OQREAL,DIGIT
      EXTERNAL DIGIT
C
      LOGICAL OUT
C
      COMMON /CCDEFNTNR/ICODE,JJ,POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE,QREAL,OQREAL,OUT
C
      IF (CODE.EQ.-1.OR.CODE.EQ.1)THEN
         ICODE=CODE
         QREAL=.TRUE.
         OQREAL=.FALSE.
         NEED=.FALSE.
C
         POSSG=0
         POSNR=0
         POSDE=0
         POSDD=0
C
         NRSGN=0
         NRNRR=0
         NRDES=0
         NRDDE=0
C
         JJ=1000
C
         CODE=0
C
      ENDIF
C
      IF (ICODE.EQ.1)THEN
C
C     GOING FORWARDS
C
         JJ=JJ+1
         IF (QREAL) THEN
            IF (CHAR.EQ.'.') THEN
               NRDES=NRDES+1
               POSDE=JJ
               QREAL=(NRDES.LE.1.AND.NRDDE.LT.1)
               NEED=(POSDE.LE.(1+NRSGN))
            ELSEIF (CHAR.EQ.'D' .OR. CHAR.EQ.'E') THEN
               NRDDE=NRDDE+1
               POSDD=JJ
               QREAL=(NRDDE.LE.1 .AND. POSDD.GT.(NRDES+NRSGN+1))
               NEED=.TRUE.
            ELSEIF (CHAR.EQ.'-'.OR.CHAR.EQ.'+') THEN
               NRSGN=NRSGN+1
               POSSG=JJ
               QREAL=((NRSGN.LE.1.AND.POSSG.EQ.1) .OR.
     &              (NRSGN.LE.2.AND.NRDDE.EQ.1
     &              .AND.POSDD.EQ.(POSSG-1)))
               NEED=.TRUE.
            ELSE
               QREAL=(DIGIT(CHAR))
               NEED=.FALSE.
               IF (QREAL) POSNR=JJ
            ENDIF
         ENDIF
C
         OUT=QREAL
C
      ELSEIF (ICODE.EQ.-1)THEN
C
C     GOING BACKWARDS
C
         JJ=JJ-1
         IF (QREAL) THEN
            IF (CHAR.EQ.'.') THEN
               NRDES=NRDES+1
               POSDE=JJ
               QREAL=(NRDES.LE.1)
               IF (QREAL) THEN
                  QREAL=(POSDE+1.NE.POSSG)
               ENDIF
               IF (QREAL.AND.NRSGN.GT.0)
     &              QREAL=(.NOT.(POSSG.EQ.JJ+1))
            ELSEIF (CHAR.EQ.'D' .OR.
     &              CHAR.EQ.'E') THEN
               NRDDE=NRDDE+1
               POSDD=JJ
               QREAL=(NRDDE.LE.1.AND.NRDES.LT.1.AND.
     &              ((NRSGN.EQ.1.AND.POSDD+1.EQ.POSSG).OR.
     &              (NRSGN.EQ.0)))
               NEED=.TRUE.
            ELSEIF (CHAR.EQ.'-'.OR.
     &              CHAR.EQ.'+') THEN
               NRSGN=NRSGN+1
               POSSG=JJ
               QREAL=((NRSGN.EQ.1.OR.(NRSGN.EQ.2.AND.NRDDE.EQ.1))
     &              .AND.(POSSG+1.EQ.POSDE.OR.POSSG+1.EQ.POSNR))
               NEED=.FALSE.
            ELSE
               QREAL=((DIGIT(CHAR)).AND.NRSGN.LE.1)
               IF (QREAL) THEN
                  POSNR=JJ
                  QREAL=(POSNR+1.NE.POSSG)
               ENDIF
               NEED=.FALSE.
            ENDIF
         ENDIF
C
         OUT=QREAL
C
      ELSE
         WRITE(*,*) MYNAME,'CODE UNKNOWN',CODE,'.'
      ENDIF
C
      DEFNTNR=OUT
      RETURN
      END
#__file: 'digit.F' 0100664    **DO NOT DELETE**
      LOGICAL FUNCTION DIGIT(STRING)
C     ***********************************************************************
C     +                                                                     *
C     CHECKS IF STRING IS A DIGIT                                           *
C     +                                                                     *
CI    STRING (C*1) = STRING TO BE EXAMINED                                  *
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
      CHARACTER*1 STRING
C
      LOGICAL BUFF
C
      BUFF= (  STRING.EQ.'0' .OR.
     &     STRING.EQ.'1' .OR.
     &     STRING.EQ.'2' .OR.
     &     STRING.EQ.'3' .OR.
     &     STRING.EQ.'4' .OR.
     &     STRING.EQ.'5' .OR.
     &     STRING.EQ.'6' .OR.
     &     STRING.EQ.'7' .OR.
     &     STRING.EQ.'8' .OR.
     &     STRING.EQ.'9')
C
      DIGIT=BUFF
C
      RETURN
      END
#__file: 'dj2000.F' 0100664    **DO NOT DELETE**
      SUBROUTINE DJ2000(DAY,YY,MM,DD,HH,MI,SEC)
CP  COMPUTES CALENDER DATE FROM MODIFIED JULIAN DAY 2000
C   VALID FOR DATES BETWEEN 1950/JAN/1 AND 2099/DEC/31.
C
C   MJD(2000) = MJD(1950) - 18262.0 IS = 0 ON 2000/01/01 AT 00:00:00.
C
CI  (REAL) DAY = MOD. JUL. DAY, REFERRED TO 2000.
CO  (INT*4) YY = YEAR WITH 4 DIGITS
CO  (INT*4) MM = MONTH
CO  (INT*4) DD = DAY
CO  (INT*4) HH = HOUR
CO  (INT*4) MI = MINUTE
CO  (REAL) SEC = SECOND.
C
      IMPLICIT NONE
C
      CHARACTER*40 SCCS_INFO
      DATA SCCS_INFO
     & /'@(#)dj2000.f \n'/
C
      REAL DAY, SEC
      INTEGER YY,MM,DD,HH,MI
C
      REAL Z,A,ALPHA,B
      INTEGER IC,ID,IE
C
C  MAKE SURE TO ROUND-OFF ONLY DOWN, ALSO FOR NEGATIVE MJD:
      Z = NINT(-0.5D0+DAY) + 2451545 ! true julian date
      IF (Z .LT. 2299161D0) THEN
         A = Z
      ELSE
         ALPHA = INT((Z - 1867216.25D0) / 36524.25D0)
         A = Z + 1.0D0 + ALPHA - INT(ALPHA / 4.0D0)
      END IF
      B = A + 1524.0D0
      IC = INT((B - 122.1D0) / 365.25D0)
      ID = INT(365.25D0 * IC)
      IE = INT((B - ID) / 30.6001D0)
      DD = B - ID - INT(30.6001D0 * IE)
      IF (IE .LT. 14) THEN
         MM = IE - 1
      ELSE
         MM = IE - 13
      END IF
      IF (MM .GT. 2) THEN
         YY = IC - 4716
      ELSE
         YY = IC - 4715
      END IF
C
      SEC = (DAY - NINT(-0.5D0+DAY))*24.D0
      HH = SEC ! always positive
      SEC = (SEC - DFLOAT(HH))*6.D1
      MI = SEC ! always positive
      SEC = (SEC - DFLOAT(MI))*6.D1
      RETURN
      END
#__file: 'eval.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDEVAL
      IMPLICIT NONE
      LOGICAL ACTIVE,BDEB
      COMMON /CCEVAL/BDEB,ACTIVE
      DATA ACTIVE/.false./
      END BLOCK DATA


      SUBROUTINE EVAL(STRING,N,NRVAR,NAMVAR,VALVAR,VARLEN,
     &     LFL,IRC)
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
      INTEGER II,CNT,LENS,LENGTH
      EXTERNAL LENGTH
      LOGICAL BDEB,ACTIVE
      LOGICAL CHANGED,CHNG(5),LFL(5)
      CHARACTER*16 MYNAME
      DATA MYNAME /'EVAL'/
      COMMON /CCEVAL/BDEB,ACTIVE
C
C     Debug System.
C
      BDEB=.false.
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C
C-----REMOVE UNNECCESARY BLANKS
C
      CALL CHOP0(STRING,N)
C
      IF (BDEB) THEN
         LENS=LENGTH(STRING,N,0)
         WRITE(*,*) MYNAME,'Debug:',(STRING(II:II),II=1,LENS)
      ENDIF
C
C-----INITIALISE
C
      CNT=0
      IRC=0
      CHANGED=.TRUE.
      DO WHILE(CHANGED)
C
C------VARIABLE SUBSTITUTION
C
         IF (LFL(1)) CALL EVALV(STRING,N,NRVAR,NAMVAR,VALVAR,VARLEN,
     &        CHNG(1),IRC)
         IF (IRC.NE.0) RETURN
C
C------FUNCTION EVALUATION
C
         IF (LFL(2)) CALL EVALF(STRING,N,CHNG(2),IRC)
         IF (IRC.NE.0) RETURN
C
C------MATHEMATICAL PROCESSING
C
         IF (LFL(3)) CALL EVALM(STRING,N,CHNG(3),IRC)
         IF (IRC.NE.0) RETURN
C
C------LOGICAL OPERATIONS
C
         IF (LFL(4)) CALL EVALL(STRING,N,CHNG(4),IRC)
         IF (IRC.NE.0) RETURN

C
C------RECURSIVE PROCESSING
C
         CHANGED=.FALSE.
         DO II=1,4
            IF(CHNG(II).AND.LFL(II)) CHANGED=LFL(5)
         ENDDO
C
C------UPDATE COUNTER AND MAKE SURE WE DON'T LOOP FOREVER
C
         IF (CNT.LT.250) THEN
            CNT=CNT+1
         ELSE
            IRC=999
            CHANGED=.FALSE.
            WRITE(*,*) MYNAME,'Error: recursive variable call.'
         ENDIF
      ENDDO
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine ends.',IRC
C
      RETURN
      END
#__file: 'evalf.F' 0100664    **DO NOT DELETE**
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
#__file: 'evall.F' 0100664    **DO NOT DELETE**
      SUBROUTINE EVALL(STRING,N,CHANGED,IRC)
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
      CHARACTER(len=N) STRING
      CHARACTER(len=N) STRING2
      LOGICAL CHANGED
      CHARACTER*1 DEL(2)
      CHARACTER*1 BUFF
C
      INTEGER LEV,POS(2),LENS,LENGTH,II,DELL
      EXTERNAL LENGTH
      LOGICAL DONE
      COMMON /CCEVALL/ LENS
C
      CHANGED=.FALSE.
C
C-----PERFORM ALL LOGICAL OPERATIONS
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
            ELSE
C     EXECUTE CALC, AND REPLACE PARANTHESIS WITH []
               STRING(POS(1):POS(1))='['
               STRING(POS(2):POS(2))=']'
               CALL EXECL(STRING,N,CHANGED,POS)
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
#__file: 'evalm.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDEVALM
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCEVALM/ LENS
      DATA LENS /1/
      END BLOCK DATA


      SUBROUTINE EVALM(STRING,N,CHANGED,IRC)
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
      CHARACTER*1 DEL(2)
C
      INTEGER LEV,POS(2),LENS,LENGTH,II,DELL
      EXTERNAL LENGTH
      LOGICAL DONE
      CHARACTER*1 BUFF
C
      COMMON /CCEVALM/ LENS
C
      CHANGED=.FALSE.
C
C-----PERFORM ALL MATHEMATICAL OPERATIONS
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
               CALL EXECM(STRING,N,CHANGED,POS)
            ELSE
C     EXECUTE CALC, AND REPLACE PARANTHESIS WITH []
               STRING(POS(1):POS(1))='['
               STRING(POS(2):POS(2))=']'
               CALL EXECM(STRING,N,CHANGED,POS)
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
      CALL TRUNCX0(STRING,N)
C
      IRC=0
C
      RETURN
      END
#__file: 'evalv.F' 0100664    **DO NOT DELETE**
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
#__file: 'execf.F' 0100664    **DO NOT DELETE**
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
#__file: 'execl.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDEXECL
      IMPLICIT NONE
      LOGICAL FIRST
      INTEGER NROP,LENS,LENB
      PARAMETER(NROP=9)
      CHARACTER*2 COP(NROP)
      INTEGER IOP(NROP)
      COMMON /CCEXECL/COP,IOP,FIRST,LENS,LENB
      DATA FIRST /.TRUE./,LENS/1/,LENB /1/
      END BLOCK DATA


      SUBROUTINE EXECL(STRING,N,CHANGED,POS)
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
     &     BPOS(MAXO,2),MAXB,
     &     CODE,START,END,II,JJ,KK,LEVEL
      EXTERNAL LENGTH
      LOGICAL FIRST,CLEAN,LAST,DONE,FOUND
      INTEGER NROP
      PARAMETER(NROP=9)
      CHARACTER*2 COP(NROP)
      INTEGER IOP(NROP)
C
      COMMON /CCEXECL/COP,IOP,FIRST,LENS,LENB
C
      IF (FIRST)THEN
C
C     DEFINITION OF OPERATION STRING
C
         COP(1)='=='
         COP(2)='>='
         COP(3)='<='
         COP(4)='<>'
         COP(5)='>'
         COP(6)='<'
         COP(7)='!'
         COP(8)='&&'
         COP(9)='||'
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
C     FIND ALL OPERATIONS
C
            LENS=LENGTH(BUFF1000,N,LENS)
            CALL CHOP0(BUFF1000,LENS)
            LENS=LENGTH(BUFF1000,N,LENS)
C
            LEVEL=0
            MAXB=0
            II=1
            DO WHILE(II.LE.LENS)
               IF (BUFF1000(II:II).EQ.'(') LEVEL=LEVEL+1
               IF (BUFF1000(II:II).EQ.')') LEVEL=LEVEL-1
               IF (BUFF1000(II:II).EQ.'[') LEVEL=LEVEL+1
               IF (BUFF1000(II:II).EQ.']') LEVEL=LEVEL-1
               IF (BUFF1000(II:II).EQ.'{') LEVEL=LEVEL+1
               IF (BUFF1000(II:II).EQ.'}') LEVEL=LEVEL-1
               IF (LEVEL.EQ.0) THEN
                  JJ=0
                  FOUND=.TRUE.
                  DO WHILE (JJ.LT.IOP(KK).AND.
     &                 (II+JJ-1).LE.LENS.AND.FOUND)
                     JJ=JJ+1
                     FOUND=(BUFF1000(II+JJ-1:II+JJ-1).EQ.
     &                    COP(KK)(JJ:JJ))
                  ENDDO
                  IF (FOUND.AND.(II+JJ-1).LE.LENS) THEN
                     IF (MAXB.LT.MAXO) MAXB=MAXB+1
                     BPOS(MAXB,1)=II
                     BPOS(MAXB,2)=II+IOP(KK)-1
                     II=II+IOP(KK)-1
                  ENDIF
               ENDIF
               II=II+1
            ENDDO
C
C     EXECUTE OPERATIONS
C
            DO II=1,MAXB
               CPOS=BPOS(II,2)+1
               CODE=1
               CALL OBJECT(BUFF1000,N,CPOS,OPOS(1,2),CODE)
               END=CPOS
C
               IF (CODE.EQ.0) THEN
C     IF WE HAD A '!', THERE MAY NOT BE ANY PRECEDING OBJECTS...
                  IF (KK.EQ.7) THEN
                     START=BPOS(II,1)
                     CALL RESET(BUFF1,250)
                  ELSE
                     CPOS=BPOS(II,1)-1
                     CODE=-1
                     CALL OBJECT(BUFF1000,N,CPOS,OPOS(1,1),CODE)
                     START=CPOS
                     CALL CEQUAL (BUFF1,250,BUFF1000,
     &                    OPOS(1,1),OPOS(2,1))
                  ENDIF
                  IF (CODE.EQ.0) THEN
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
                     CALL SLAVEL(BUFF1,BUFF2,BUFF3,KK,CLEAN,LCHNG,IRC)
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
                           IF (BPOS(JJ,2).LE.START .OR.
     &                          BPOS(JJ,1).GT.END) THEN
                              BPOS(JJ,1)=BPOS(JJ,1)+LENB
                              BPOS(JJ,2)=BPOS(JJ,2)+LENB
                           ELSE
                              BPOS(JJ,1)=BPOS(MAXB,1)
                              BPOS(JJ,2)=BPOS(MAXB,2)
                              MAXB=MAXB-1
                              JJ=JJ-1
                           ENDIF
C     REMOVE JOB IF IT FALLS OUTSIDE BUFFER
                           IF (BPOS(JJ,2).GT.N) THEN
                              BPOS(JJ,1)=BPOS(MAXB,1)
                              BPOS(JJ,2)=BPOS(MAXB,2)
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
#__file: 'execm.F' 0100664    **DO NOT DELETE**
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
#__file: 'fill0.F' 0100664    **DO NOT DELETE**
C Library: strgen4 $Id: file.f,v 1.1 2012-12-10 12:03:54 franktt Exp $
C
      SUBROUTINE FILL0(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     INSERTS SURPLUS BLANKS IN A STRING                                    *
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME        |   DATE   |                 REASON                | *
C     --------------------------------------------------------------------- *
C     | F. TVETER      | 18/07/95 | NEW                                   | *
C     | T. WILHELMSSON | 24/11/06 | Remove SAVE statements                | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER N
      CHARACTER*(*) STRING
      CHARACTER*1 C1
      INTEGER II,JJ
C
      C1=CHAR(0)
C
      JJ=0
      II=0
      DO WHILE(II.LT.N)
         II=II+1
         IF (STRING(II:II).EQ.C1) STRING(II:II)=' '
      ENDDO
      RETURN
      END
#__file: 'fill.F' 0100664    **DO NOT DELETE**
      SUBROUTINE FILL(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     INSERTS SURPLUS BLANKS IN A STRING                                    *
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
      CHARACTER*1 C1
      INTEGER II,JJ
C
      C1=CHAR(0)
      JJ=0
      II=0
      DO WHILE(II.LT.N)
         II=II+1
         IF (STRING(II:II).EQ.C1) STRING(II:II)=' '
      ENDDO
      RETURN
      END
#__file: 'ftunit.F' 0100664    **DO NOT DELETE**
      INTEGER FUNCTION FTUNIT(IRC)
C***********************************************************************
C                                                                      *
C WRITTEN/MODIFIED BY:                                                 *
C--------------------------------------------------------------------- *
C|    NAME      |   DATE   |                 REASON                  | *
C--------------------------------------------------------------------- *
C| L. BEHRENDT  | 04/03/91 | NEW.                                    | *
C|              |          |                                         | *
C--------------------------------------------------------------------- *
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER IRC
C
      CHARACTER*16 MYNAME
      INTEGER UN,maxun
      data maxun /91/
      LOGICAL FOUND,T,F
C
      DATA F       /.FALSE./
      DATA T       /.TRUE./
C
      DATA MYNAME /'FTUNIT'/
C
C
C------- Find an unused unit number between 30 and 99.
C
C      un=maxun
C      maxun=maxun-1
C      ftunit=un
C      write(*,*) myname,'Hardcoding unit number to:',UN
C      return
C
      UN=MAXUN
      FOUND=T
      DO WHILE (FOUND.AND.UN.GE.30)
         UN=UN-1
         INQUIRE(UNIT=UN,OPENED=FOUND)
C     WRITE(*,*) MYNAME,UN,FOUND
      ENDDO
      IF (FOUND) THEN
         WRITE(*,*) MYNAME,': ----------- WARNING ------------'
         WRITE(*,*) MYNAME,': NO UNUSED UNITS FOUND (30 TO 99)'
         WRITE(*,*) MYNAME,': ----------- WARNING ------------'
         IRC=107
      ELSE
         FTUNIT=UN
      ENDIF
C
      RETURN
      END
#__file: 'getcom.F' 0100664    **DO NOT DELETE**
      SUBROUTINE GETCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,IRC)
C
      IMPLICIT NONE
C
      INTEGER MAXCOM, NRCOM
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      INTEGER CMLEN(MAXCOM),IRC
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'GETCOM'/
      INTEGER INRCOM,II,LENGTH
      EXTERNAL LENGTH
      DATA INRCOM /13/
C
C     DEFINE COMMANDS
C
      NRCOM=INRCOM
      IF (MAXCOM.LT.INRCOM) THEN
         WRITE(*,*) MYNAME,'System error.'
         IRC=999
         RETURN
      END IF
C     
      COM20(1)='if'
      HLP20(1)='(<expr>)'
      COM20(2)='elseif'
      HLP20(2)='(<expr>)'
      COM20(3)='else'
      CALL RESET(HLP20(3),20)
      COM20(4)='endif'
      CALL RESET(HLP20(4),20)
      COM20(5)='!'
      HLP20(5)='<UNIX command>'
      COM20(6)='include'
      HLP20(6)='(<file>)'
      COM20(7)='stop'
      CALL RESET(HLP20(7),20)
      COM20(8)='exit'
      HLP20(8)='<exit code>'
      COM20(9)='echo'
      HLP20(9)='<text>'
      COM20(10)='set'
      HLP20(10)='<var>=<expr[$(var)]>'
      COM20(11)='import'
      HLP20(11)='<var1> <var2> ...'
      COM20(12)='xx'
      CALL RESET(HLP20(12),20)
      COM20(13)='x'
      CALL RESET(HLP20(13),20)
C
      DO II=1,NRCOM
         CALL CHOP0(COM20(II),20)
         CALL CHOP0(HLP20(II),20)
C     FIND LENGTH OF COMMAND
         CMLEN(II)=LENGTH(COM20(II),20,3)
C     write(*,*) COM20(II)(1:CMLEN(II))
      ENDDO
C
C     
      RETURN
      END
#__file: 'getflags.F' 0100664    **DO NOT DELETE**
      SUBROUTINE GETFLAGS(BUFF250,
     &           PPDAT,
     &           IRC)
C
      IMPLICIT NONE
C
      INTEGER NROPT
      PARAMETER(NROPT=5)
C
      character*250 buff250
      LOGICAL PPDAT(NROPT)
      integer irc
C
      CHARACTER*1 OPT,OPTL(NROPT)
      DATA OPTL /'V','F','M','L','R'/
C     
      logical bdone,bbdone,found
      integer lenb, length,kk,jj
      external length
      CHARACTER*16 MYNAME
      DATA MYNAME /'GETFLAGS'/
C     
C     CHECK FOR PRE-PROCESSING
C     
      DO KK=1,NROPT
         PPDAT(KK)=.FALSE.
      ENDDO
C     
C     CHECK FOR PRE-PROCESSOR FLAGS
C     
      LENB=LENGTH(BUFF250,250,10)
      JJ=1
      BDONE=(JJ.GT.LENB)
      DO WHILE (.NOT.BDONE)
         OPT=BUFF250(JJ:JJ)
         FOUND=.FALSE.
         KK=1
         BBDONE=(KK.GT.NROPT)
         DO WHILE (.NOT.BBDONE)
            IF (OPT.EQ.OPTL(KK)) THEN
               PPDAT(KK)=.TRUE.
               FOUND=.TRUE.
               BBDONE=.TRUE.
            ELSE
               KK=KK+1
               BBDONE= (KK.GT.NROPT) 
            ENDIF
         END DO
         IF (.NOT.FOUND) THEN
            BDONE=.TRUE.
         ELSE
            JJ=JJ+1
            BDONE=(JJ.GT.LENB)
         END IF
      END DO
C
      BUFF250=BUFF250(MAX(1,JJ):LENB)
      call chop0(buff250,250)
C
      RETURN
      END 
#__file: 'getgrp.F' 0100664    **DO NOT DELETE**
      subroutine GETGRP(BUFF250,chdr,
     &     MAXGRP,MINGRP,NRGRP,GROUPS,
     &     IRC)
      implicit none
C
      character*250 buff250
      integer chdr
      integer maxgrp
      integer mingrp
      integer nrgrp
      integer groups(6,maxgrp) ! 1=parent, 2=type, 3=id/chdr,4=lvl
      integer irc
C
      integer cnrgrp,fii,ii,fcii,cii,jj,copt,chii,tii,nchii
      logical bdone,bbdone,bcom,bnum,bnew
      integer cgroups(6,maxgrp) ! 1=parent, 2=next sibling, 3=first child,  4=type, 5=id/chdr, 6=lvl
      integer lenh,length,num,tnum
      external length
      character*1 opt
C      
      character*16 myname
      data myname /'GETGRP'/
C
C     CHECK FOR GROUP FLAGS
C     
      LENH=LENGTH(BUFF250,250,10)
      JJ=1
      BNEW=.TRUE.
      BBDONE=.false.            ! always enter loop
      DO WHILE (.NOT.BBDONE)
C
C     make next group chain for this header
C
         copt=2                 ! default type is "%"
         cnrgrp=0
         BDONE=(jj.gt.lenh)
         DO WHILE (.NOT.BDONE)
            OPT=BUFF250(JJ:JJ)
            BCOM=.FALSE.
            BNUM=.FALSE.
            IF (OPT.EQ.'&') THEN ! mandatory
               BCOM=.TRUE.
               COPT=1
            ELSE IF (OPT.EQ.'%') THEN ! 1 or 0
               BCOM=.TRUE.
               COPT=2
            ELSE IF (OPT.EQ.'$') THEN ! 1 or 0 or absent
               BCOM=.TRUE.
               COPT=3
            ELSE IF (OPT.EQ.'0') THEN 
               BNUM=.TRUE.
               NUM=0
            ELSE IF (OPT.EQ.'1') THEN 
               BNUM=.TRUE.
               NUM=1
            ELSE IF (OPT.EQ.'2') THEN 
               BNUM=.TRUE.
               NUM=2
            ELSE IF (OPT.EQ.'3') THEN 
               BNUM=.TRUE.
               NUM=3
            ELSE IF (OPT.EQ.'4') THEN 
               BNUM=.TRUE.
               NUM=4
            ELSE IF (OPT.EQ.'5') THEN 
               BNUM=.TRUE.
               NUM=5
            ELSE IF (OPT.EQ.'6') THEN 
               BNUM=.TRUE.
               NUM=6
            ELSE IF (OPT.EQ.'7') THEN 
               BNUM=.TRUE.
               NUM=7
            ELSE IF (OPT.EQ.'8') THEN 
               BNUM=.TRUE.
               NUM=8
            ELSE IF (OPT.EQ.'9') THEN 
               BNUM=.TRUE.
               NUM=9
            ELSE IF (OPT.EQ.' ') THEN 
               BNEW=.TRUE.
            ELSE
               WRITE(*,*) MYNAME,
     &              'Error: unknown header option, ',
     &              OPT,' in '//buff250(1:lenh)
               IRC=999
               RETURN
            END IF
            IF (BCOM) THEN      ! new group
               TNUM=0
               cNRGRP=cNRGRP+1
               IF (cNRGRP.GT.MAXGRP) THEN
                  WRITE(*,*) MYNAME,
     &                 'Error: too many groups in '//buff250(1:lenh)
                  irc=932
                  return
               END IF
               cgroups(1,cnrgrp)=cnrgrp-1 ! parent
               cgroups(2,cnrgrp)=0 ! next sibling
               cgroups(3,cnrgrp)= 0 ! first child
               cgroups(4,cnrgrp)=copt ! type
               cgroups(5,cnrgrp)=0 ! id/hdr nr
               cgroups(6,cnrgrp)=cnrgrp ! level
               if (cgroups(1,cnrgrp).gt.0) then ! update parent
                  cgroups(3,cgroups(1,cnrgrp))=cnrgrp ! first child
               end if
               bnew=.false.
            ELSE IF (BNUM) THEN !
               IF (BNEW) THEN
                  WRITE(*,*) MYNAME,
     &                 'Error: invalid group definitions in"'//
     &                 buff250(1:lenh),'" header no:',chdr
                  irc=974
                  return
               END IF
               TNUM=TNUM*10+NUM
               cgroups(5,cnrgrp)=tnum ! id/hdr nr
            END IF
            jj=jj+1
            BDONE=(BNEW.OR.JJ.GT.LENH)
         END DO
C     
C     define default groups
C
         if (cnrgrp.eq.0) then
            cnrgrp=1
            copt=2              ! default is the "optional group"
            cgroups(1,cnrgrp)=cnrgrp-1 ! parent
            cgroups(2,cnrgrp)=0 ! no sibling
            cgroups(3,cnrgrp)=0 ! child
            cgroups(4,cnrgrp)=copt ! type is "optional group"
            cgroups(5,cnrgrp)=0 ! no id yet
            cgroups(6,cnrgrp)=cnrgrp ! top level (=0)
         end if

         do ii=1,cnrgrp
            if (cgroups(5,II).eq.0) then
               mingrp=mingrp-1
               cgroups(5,II)=mingrp
            end if
         end do
C
C     add final header group (which must be unique)
C
         cnrgrp=cnrgrp+1
         if (cnrgrp.gt.maxgrp) then
            WRITE(*,*) MYNAME,
     &           'Error: too many groups ',cnrgrp
            irc=984
            return
         end if
         cgroups(1,cnrgrp)=cnrgrp-1    ! parent
         cgroups(2,cnrgrp)=0    ! no siblings
         cgroups(3,cnrgrp)=0    ! no children
         cgroups(4,cnrgrp)=-1     ! type
         cgroups(5,cnrgrp)=-chdr  ! id
         cgroups(6,cnrgrp)=cnrgrp ! level
         if (cgroups(1,cnrgrp).gt.0) then ! update parent
            cgroups(3,cgroups(1,cnrgrp))=cnrgrp ! first child
         end if
C
C     find group from head down in archive
C
         fcii=0
         fii=0
         ii=1
         cii=1
         bdone=(ii.gt.nrgrp)
         do while (.not.bdone)
            if (groups(1,ii).eq.cgroups(1,cii).and. ! same parent
     &           groups(4,ii).eq.cgroups(4,cii).and. ! same type
     &           groups(5,ii).eq.cgroups(5,cii).and. ! same id
     &           groups(6,ii).eq.cgroups(6,cii)) then ! same lvl
               fii=ii
               fcii=cii
               if (cii.eq.cnrgrp) then ! this is last group
                  write(*,*) myname,'This should never happen...'
                  irc=999
                  return
               end if
               cii=cii+1
               cgroups(1,cii)=ii ! redefine the parent of first child
C     
C     a child may never be before the parent in archive,
C     we do therefore not need to reset ii...
C
            else
               ii=ii+1
               bdone=(ii.gt.nrgrp)
            end if
         end do
C
C     add last part of chain not found in archive
C
         tii=nrgrp+1            ! the top of the added chain
         do cii=fcii+1,cnrgrp
            nrgrp=nrgrp+1
            if (nrgrp.gt.maxgrp) then
               WRITE(*,*) MYNAME,
     &              'Error: too many groups ',nrgrp
               irc=984
               return
            end if
            groups(1,nrgrp)=fii !parent
            groups(2,nrgrp)=0 ! sibling defined later for top level
            if (cgroups(3,cii).ne.0) then
               groups(3,nrgrp)=nrgrp+1 ! child is next object
            else
               groups(3,nrgrp)=0 ! child
            end if
            groups(4,nrgrp)=cgroups(4,cii) ! type
            groups(5,nrgrp)=cgroups(5,cii) ! id
            groups(6,nrgrp)=cgroups(6,cii) ! lvl
            fii=nrgrp
         end do
C
C     add to sibling-chain or update parents first child-index
C
         chii=0
         if (groups(1,tii).gt.0) then ! there exists a parent
            nchii=groups(3,groups(1,tii)) !parent's first child
         else
            nchii=groups(2,1) ! sibling of first top-level object
         end if
         bdone=(nchii.eq.0)     ! parent has no other children?
         do while (.not.bdone)
            chii=nchii
            nchii=groups(2,chii)
            bdone=(nchii.eq.0)  ! parent has no other children?
         end do
         if (chii.eq.0) then    ! parents first child is this one
            if (groups(1,tii).eq.0) then
               if (tii.gt.1) 
     &              groups(2,1)=tii ! first sibling of top object
            else
               groups(3,groups(1,tii))=tii ! first child of parent
            end if
         else                
            groups(2,chii)=tii  ! last child in a row
         end if
C     
      BBDONE=(JJ.GT.LENH)
      END DO
C     
      return
      end 






#__file: 'getind.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDGETIND
      IMPLICIT NONE
      LOGICAL ACTIVE,BDEB
      COMMON /CCGETIND/ACTIVE
      DATA ACTIVE/.false./
      END BLOCK DATA


      SUBROUTINE GETIND(IKODE,TYPE,SCNAME,AUX,CID250,CDR250,CLK250,
     &     IRC)
C
      IMPLICIT NONE
C
      INTEGER IKODE           ! -1=FILE WILL BE CREATED, 0=FILE MUST EXIST
      CHARACTER*12 TYPE,SCNAME,AUX
      CHARACTER*250 FILENM,CID250, CDR250, CLK250
      INTEGER IRC
C
      INTEGER NRHDR,LINE
      PARAMETER (NRHDR=250)
      CHARACTER*250 HDR250(NRHDR),DAT250,
     &     BUFF250,NUKEHEAD
      EXTERNAL NUKEHEAD
      INTEGER INTOUT
      LOGICAL ENDOFF
C
      LOGICAL LFLDAT(3),ACTIVE,BDEB,OK,FOUND,DONE
      INTEGER UNITI,FTUNIT,LENGTH,LENI,LEND,LENT,LENN,LENA,LENH,LENB
      EXTERNAL FTUNIT,LENGTH
      INTEGER MAXSAT,NRSAT,II,KODE
      PARAMETER (MAXSAT = 20)
      CHARACTER*250 SAT250(MAXSAT),SAT250B
      CHARACTER*12 SNAM12(MAXSAT),AUX12(MAXSAT),SNAM12B,AUX12B
      CHARACTER*16 MYNAME
C
      DATA MYNAME /'GETIND'/
      COMMON /CCGETIND/ACTIVE
C
      BDEB=.false.
C
      CALL CHOP0(SCNAME,12)
      LENN=LENGTH(SCNAME,12,3)
      CALL CHOP0(AUX,12)
      LENA=LENGTH(AUX,12,3)
C
      IF (LENN.EQ.0) THEN
         WRITE(*,*) MYNAME,'Invalid spacecraft name (length=0).'
         IRC=456
         RETURN
      END IF
C
      FILENM=CID250
      UNITI = FTUNIT(IRC)
      IF (IRC.NE.0) RETURN
      LENI = LENGTH(CID250,250,10)
      IF (IKODE.EQ.-1) THEN     ! CREATE FILE IF IT DOES NOT EXIST
         OPEN(UNIT=UNITI, FILE=CID250, STATUS='UNKNOWN',
     &        FORM='FORMATTED',IOSTAT=IRC)
      ELSE                      ! FILE MUST EXIST
         OPEN(UNIT=UNITI, FILE=CID250, STATUS='OLD',
     &        FORM='FORMATTED',IOSTAT=IRC)
      END IF
      IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'ERROR OPENING:',
     &        CID250(1:LENI)
         RETURN
      END IF
C
      LFLDAT(1)=.FALSE.
      LFLDAT(2)=.FALSE.
      LFLDAT(3)=.FALSE.
      NRSAT=0
C
      DO II=1,NRHDR
         HDR250(II) = ''
      ENDDO
C
      HDR250(1)='GETIND INDEX FILE V1.0 [0]'
      HDR250(2)='LIST OF SATELLITES AND INDEXED FILES [*]VFLR %'
      HDR250(3)='LIST OF SATELLITES, CATEGORIES AND'//
     &     ' INDEXED FILES [*]VFLR %'
C
      if (bdeb) then
         WRITE(*,*) MYNAME,'----------------------------------------'
         WRITE(*,*) MYNAME,'Reading index file:'//
     &        CID250(1:LENI)
      end if
C
C     READ DATA FROM INPUT FILE..............................
C
      KODE=-1
      CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,ENDOFF,IRC)
      IF (IRC.NE.0.AND.IKODE.EQ.-1) THEN
         WRITE(*,*) MYNAME,'Attempting to create index file.'
         IRC=0
      ELSE IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'Error return from NUKEM.'
         RETURN
      ELSE
C     
         LINE=INTOUT
C     
         KODE=0
         DO WHILE (.NOT.ENDOFF)
C     
C     READ NEXT DATA LINE
C     
            CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,
     &           ENDOFF,IRC)
            IF (IRC.NE.0) THEN
               WRITE(*,*) MYNAME,'Error return from NUKEM.'
               RETURN
            END IF
            LINE=INTOUT
C     
            IF (BDEB) WRITE(*,*) MYNAME,'Debug: Read header:',LINE
C     
C     CHECK WHAT LINE WE JUST READ
C     
            IF (LINE.EQ.1) THEN ! GETIND V1.0
               LFLDAT(1) =.TRUE.
            ELSEIF (LINE.EQ.2) THEN ! SATELLITE NAME
               IF (LFLDAT(3)) THEN
                  WRITE(*,*) MYNAME,'INVALID INDEX FILE SYNTAX:'//
     &                 CID250(1:LENI)
                  IRC=345
                  RETURN
               END IF
               NRSAT=NRSAT+1
               IF (NRSAT.GT.MAXSAT) THEN
                  WRITE(*,*) MYNAME,'TOO MANY SATELLITES'
                  IRC=88
                  RETURN
               END IF
C     
               buff250 = NUKEHEAD(DAT250,250)
               SNAM12(NRSAT)= buff250(1:12)
               SAT250(NRSAT)=DAT250
C     
               DO II=NRSAT,2,-1
                  IF (SNAM12(II).LT.SNAM12(II-1)) THEN
                     SNAM12B=SNAM12(II)
                     SAT250B=SAT250(II)
                     SNAM12(II)=SNAM12(II-1)
                     SAT250(II)=SAT250(II-1)
                     SNAM12(II-1)=SNAM12B
                     SAT250(II-1)=SAT250B
                  END IF
               END DO
C     
               LFLDAT(2) =.TRUE.
            ELSEIF (LINE.EQ.3) THEN ! SATELLITE NAME+AUX
               IF (LFLDAT(2)) THEN
                  WRITE(*,*) MYNAME,'INVALID INDEX FILE SYNTAX:'//
     &                 CID250(1:LENI)
                  IRC=345
                  RETURN
               END IF
               NRSAT=NRSAT+1
               IF (NRSAT.GT.MAXSAT) THEN
                  WRITE(*,*) MYNAME,'TOO MANY SATELLITES'
                  IRC=88
                  RETURN
               END IF
C     
               buff250 = NUKEHEAD(DAT250,250)
               SNAM12(NRSAT)= buff250(1:12)
               buff250 = NUKEHEAD(DAT250,250)
               AUX12(NRSAT)= buff250(1:12)
               SAT250(NRSAT)=DAT250
C     
               DO II=NRSAT,2,-1
                  IF (SNAM12(II).LT.SNAM12(II-1)) THEN
                     SNAM12B=SNAM12(II)
                     AUX12B=AUX12(II)
                     SAT250B=SAT250(II)
                     SNAM12(II)=SNAM12(II-1)
                     AUX12(II)=AUX12(II-1)
                     SAT250(II)=SAT250(II-1)
                     SNAM12(II-1)=SNAM12B
                     AUX12(II-1)=AUX12B
                     SAT250(II-1)=SAT250B
                  END IF
               END DO
C     
               LFLDAT(3) =.TRUE.
            ELSE IF (LINE.NE.0) THEN ! LINE.EQ.0 IMPLIES SOMETIMES EOF
               WRITE(*,*) MYNAME,
     &              'System error, line not implemented:',LINE
               IRC=999
               RETURN
            ENDIF
         ENDDO
C     
         KODE=1
         CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,
     &        ENDOFF,IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from NUKEM.'
            RETURN
         END IF
C
      END IF
C     
C     MUST CHECK IF AUX IS EMPTY AND INDEX FILE EXPECTS NON-EMPTY
C     
      IF (LENA.EQ.0 .AND. LFLDAT(3)) THEN
         WRITE(*,*) MYNAME,'System error.'
         IRC=999
         RETURN
      END IF
C
      IF (LENA.NE.0 .AND. LFLDAT(2)) THEN
         WRITE(*,*) MYNAME,'System error.'
         IRC=998
         RETURN
      END IF
C
C     find location
C
      FOUND = .false.
      II=1
      DONE = (II.GT.NRSAT .OR. FOUND)
      DO WHILE (.NOT. DONE)
         IF (LENA.EQ.0) THEN
            IF (SNAM12(II).EQ.SCNAME) THEN
               CLK250 = SAT250(II)
               FOUND = .TRUE.
            ELSE
               II = II+1
            END IF
         ELSE
            IF (SNAM12(II).EQ.SCNAME .AND. AUX12(II).EQ.AUX) THEN
               CLK250 = SAT250(II)
               FOUND = .TRUE.
            ELSE
               II = II+1
            END IF
         END IF
         DONE = (II.GT.NRSAT .OR. FOUND)
      END DO
C
      IF (.NOT.FOUND .AND. IKODE.EQ.-1) THEN ! add sat if it does not exist
         FOUND = .TRUE.
         NRSAT=NRSAT+1
         IF (NRSAT.GT.MAXSAT) THEN
            WRITE(*,*) MYNAME,'TOO MANY SATELLITES'
            IRC=91
            RETURN
         END IF
         CALL CHOP0(CDR250,250)
         CALL CHOP0(TYPE,12)
         LEND=LENGTH(CDR250,250,10)
         LENT=LENGTH(TYPE,12,10)
         SNAM12(NRSAT)= SCNAME
         AUX12(NRSAT)= AUX
         IF (CDR250(LEND:LEND).EQ.'/') THEN
            IF (LENA.EQ.0) THEN
               SAT250(NRSAT)=CDR250(1:LEND)//
     &              SCNAME(1:LENN)//TYPE(1:LENT)
            ELSE
               SAT250(NRSAT)=CDR250(1:LEND)//
     &              SCNAME(1:LENN)//AUX(1:LENA)//TYPE(1:LENT)
            END IF
         ELSE
            IF (LENA.EQ.0) THEN
               SAT250(NRSAT)=CDR250(1:LEND)//'/'//
     &              SCNAME(1:LENN)//TYPE(1:LENT)
            ELSE
               SAT250(NRSAT)=CDR250(1:LEND)//'/'//
     &              SCNAME(1:LENN)//AUX(1:LENA)//TYPE(1:LENT)
            END IF
         END IF
         CALL CHOP0(SAT250(NRSAT),250)
         CLK250 = SAT250(NRSAT)
C
         DO II=NRSAT,2,-1
            IF (SNAM12(II).LT.SNAM12(II-1)) THEN
               IF (LENA.EQ.0) THEN
                  SNAM12B=SNAM12(II)
                  SAT250B=SAT250(II)
                  SNAM12(II)=SNAM12(II-1)
                  SAT250(II)=SAT250(II-1)
                  SNAM12(II-1)=SNAM12B
                  SAT250(II-1)=SAT250B
               ELSE
                  SNAM12B=SNAM12(II)
                  AUX12B=AUX12(II)
                  SAT250B=SAT250(II)
                  SNAM12(II)=SNAM12(II-1)
                  AUX12(II)=AUX12(II-1)
                  SAT250(II)=SAT250(II-1)
                  SNAM12(II-1)=SNAM12B
                  AUX12(II-1)=AUX12B
                  SAT250(II-1)=SAT250B
               END IF
            END IF
         END DO
      ELSE IF (.NOT. FOUND) THEN
         WRITE(*,*) MYNAME,'No data file found for satellite:'//
     &        SCNAME(1:LENN), '('//AUX(1:LENA)//')'
         IRC=92
         RETURN
      END IF
C
      CLOSE(UNITI)
C
C     WRITE TO FILE
C
      IF (IKODE.EQ.-1) THEN
         OPEN(UNIT=UNITI, FILE=CID250, STATUS='UNKNOWN',
     &        FORM='FORMATTED',IOSTAT=IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'ERROR OPENING:',
     &           CID250(1:LENI)
            RETURN
         END IF
C
         buff250='#!NUKE'
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         buff250='#'
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         buff250=HDR250(1)
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         buff250='#'
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         IF (NRSAT.NE.0) THEN
C
            IF (LENA.EQ.0) THEN
               buff250=HDR250(2)
            ELSE
               buff250=HDR250(3)
            END IF
            call chop0(buff250,250)
            lenB=length(buff250,250,3)
            WRITE(UNITI,*) buff250(1:lenb)
            DO II=1,NRSAT
               lenn=length(SNAM12(II),12,3)
               lena=length(AUX12(II),12,3)
               lend=length(SAT250(II),250,10)
               IF (LENA.EQ.0) THEN
                  buff250=SNAM12(II)(1:LENN)//' '//
     &                 SAT250(II)(1:MIN(LEND,250-1-LENN))
               ELSE
                  buff250=SNAM12(II)(1:LENN)//' '//
     &                 AUX12(II)(1:LENA)//' '//
     &                 SAT250(II)(1:MIN(LEND,250-2-LENN-LENA))
               END IF
               call chop0(buff250,250)
               lenB=length(buff250,250,3)
               WRITE(UNITI,*) buff250(1:lenb)
            END DO
         ELSE
            buff250='# NO SATELLITE DEFINED.'
            call chop0(buff250,250)
            lenB=length(buff250,250,3)
            WRITE(UNITI,*) buff250(1:lenb)
         END IF
C
         buff250='#'
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         CLOSE(UNITI)
C
         IF (.NOT.FOUND) THEN
            WRITE(*,*) MYNAME,'Undefined satellite:',SCNAME
            IRC=93
            RETURN
         END IF
      END IF
C
      if (bdeb) then
         WRITE(*,*) MYNAME,'----------------------------------------'
      end if
C
      RETURN
      END
#__file: 'getline.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDGETLINE
      IMPLICIT NONE
      INTEGER IUNIT
      COMMON /CCGETLINE/IUNIT
      DATA IUNIT/-1/
      END BLOCK DATA


      subroutine GETLINE(KODE,MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &     CNT,MAXHDR,CODHDR,LINE,
     &     BUFF1000,LENB,ENDOFF,IRC)
C
      implicit none
C
      INTEGER KODE
      INTEGER MAXUNIT
      INTEGER UNITA(MAXUNIT),CUNIT,LUNIT(MAXUNIT)
      CHARACTER*250 FILES(MAXUNIT)
      INTEGER CNT,MAXHDR,CODHDR(MAXHDR),LINE
      CHARACTER*1000 BUFF1000
      integer lenb
      logical endoff
      integer irc
C
      INTEGER FTUNIT, LENGTH, LEND,LENF,IUNIT
      EXTERNAL FTUNIT, LENGTH
      LOGICAL BDONE
      CHARACTER*250 BUFF250
      CHARACTER*16 MYNAME
      DATA MYNAME /'GETLINE'/
C
      COMMON /CCGETLINE/IUNIT
C
      IF (IUNIT.NE.-1 .AND. IUNIT.LT.CUNIT) THEN
C     
C     OPEN THE NEW FILE
C     
         IF (CUNIT.GT.MAXUNIT) THEN
            WRITE(*,*) MYNAME,'Too deep file nesting.'
            IRC=275
            RETURN
         END IF
C     
         LUNIT(CUNIT)=0
         UNITA(CUNIT)=FTUNIT(IRC)
         IF (IRC.NE.0) RETURN
C     
         LENF=LENGTH(FILES(CUNIT),250,10)
         OPEN(UNIT=UNITA(CUNIT),FILE=FILES(CUNIT)(1:LENF),
     &        ACCESS='SEQUENTIAL',FORM='FORMATTED',
     &        STATUS='OLD',IOSTAT=IRC)
C     
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Unable to open file:'//
     &           FILES(CUNIT)(1:LENF)
            RETURN
         END IF
      END IF
C
      IUNIT=CUNIT
C
C     READ NEXT LINE
C
      IF (KODE.EQ.-1) LUNIT(CUNIT)=0 ! reset line counter
C
      BUFF1000=''
      LENB=0
      BDONE=.FALSE.
      DO WHILE (.NOT.BDONE)
         READ(UNITA(CUNIT),'(A250)',END=99,IOSTAT=IRC) BUFF250

         LUNIT(CUNIT)=LUNIT(CUNIT)+1
         IF (IRC.NE.0) THEN
            LENF=LENGTH(FILES(CUNIT),250,10)
            WRITE(*,*) 
     &           '"'//FILES(CUNIT)(1:LENF)//'", line ',
     &           LUNIT(CUNIT),': Error: Unable to read line.'
            RETURN
         END IF
         CALL CHOP0(BUFF250,250)
         LEND=LENGTH(BUFF250,250,10)
C
C     IGNORE COMMENTS
C
         IF (LEND.NE.0 .AND. BUFF250(1:1).NE.'#') THEN
            BUFF1000(1:LENB+LEND)=BUFF1000(1:LENB)//BUFF250(1:LEND)
            LENB=LENB+LEND
            IF (BUFF1000(LENB:LENB).EQ.'~') THEN
               BUFF1000(LENB:LENB)=' '
               LENB=LENB-1
               BDONE=.FALSE.
            ELSE
               BDONE=.TRUE.
            END IF
         END IF
C
         GOTO 250
 99      CONTINUE
         IRC=0                  ! NO ERROR, JUST EOF
         IF (CUNIT.GT.1) THEN
            CLOSE(UNITA(CUNIT),IOSTAT=IRC)
C
            CUNIT=CUNIT-1
            IUNIT=CUNIT
         ELSE
C
C     END OF FILE
C
            IF (LINE.NE.-1) THEN
               IF (CNT.LT.CODHDR(LINE)
     &              .AND. CODHDR(LINE).NE.-1) THEN
                  WRITE(*,*) MYNAME,'EOF interrupts data body.'
                  IRC=174
                  RETURN
               END IF
            END IF
            ENDOFF=.TRUE.
            BDONE=.TRUE.
         END IF
C
 250     CONTINUE
      END DO
      RETURN
C
      END

      
#__file: 'getlines.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDGETLINES
      IMPLICIT NONE
      INTEGER POS
      LOGICAL BDEB
      COMMON /CCGETLINES/POS,BDEB
      DATA POS/1/,BDEB/.FALSE./
      END BLOCK DATA

      subroutine GETLINES(KODE,ARG1000,ARGLEN,MAXUNIT,LUNIT,CUNIT,
     &     CNT,MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,CODHDR,LINE,
     &     BUFF1000,LENB,ENDOFF,IRC)
C
      implicit none
C
      INTEGER KODE
      CHARACTER*1000 ARG1000
      integer arglen
      INTEGER MAXUNIT
      INTEGER LUNIT(MAXUNIT),CUNIT
      INTEGER CNT,MAXHDR,NRHDR,NRLEN(3,MAXHDR),CODHDR(MAXHDR),LINE
      LOGICAL ACTHDR(MAXHDR)
      CHARACTER*250 HDR250(MAXHDR)
      CHARACTER*1000 BUFF1000
      integer lenb
      logical endoff
      integer irc
C
      INTEGER FTUNIT, LENGTH, LEND,LENF,II,JJ,KK,LL
      EXTERNAL FTUNIT, LENGTH
      LOGICAL BBDONE,BDONE,found
      integer pos,opos,mpos
      LOGICAL BDEB
      CHARACTER*250 BUFF250
      CHARACTER*16 MYNAME
      DATA MYNAME /'GETLINES'/
C
      COMMON /CCGETLINES/POS,BDEB
C
C     READ NEXT LINE
C
      IF (KODE.EQ.-1) then
         LUNIT(CUNIT)=0         ! reset line counter
         pos=1
      end if
C
      BUFF1000=''
      LENB=0
      opos=pos ! old position (points to "next" object)
      if (bdeb)write(*,*)myname,'Opos:',opos,arg1000(opos:opos)
      endoff=pos.gt.arglen
      if (endoff) return
      bbdone=.false.
      do while (.not.bbdone)
C     search for next '-'
         found=.false.
         bdone=pos.gt.arglen
         do while(.not.bdone)
            found=arg1000(pos:pos).eq.'-'
            if (found) then
               bdone=.true.
            else
               pos=pos+1
               bdone=pos.gt.arglen
            end if
         end do
         if (found) then
            if (bdeb)write(*,*)myname,'Minus:',pos,arg1000(pos:pos)
C     we found a '-', but does it belong to a header?
C     search for first blank
            found=.false.
            mpos=pos
            bdone=pos.gt.arglen
            do while(.not.bdone)
               found=arg1000(pos:pos).eq.' '
               if (found) then
                  bdone=.true.
               else
                  pos=pos+1
                  bdone=pos.gt.arglen
               end if
            end do
            if (bdeb)write(*,*)myname,'Blank:',pos,arg1000(pos:pos)
            if (found.and.pos-1.eq.mpos) then ! stand-alone ' - ' => newline
               if (opos.eq.mpos) then ! return next item
                  pos=pos+1     ! first character after the blank
                  opos=pos
               else             ! encountered newline, return all before it...
                  buff1000=arg1000(opos:mpos-1)
                  pos=pos+1
                  LUNIT(CUNIT)=LUNIT(CUNIT)+1
                  bbdone=.true.
               end if
            else ! check if this is a header
               JJ=1
               found= .false.
               DO WHILE(.NOT.FOUND .AND. JJ.LE.MAXHDR)
                  if (ACTHDR(JJ).and.nrlen(2,jj).le.nrlen(3,jj).and.
     &                 .not.found) then ! check if we have short header
                     FOUND=((NRLEN(3,JJ)-nrlen(2,jj)+1.le.arglen-mpos)
     &                    .AND.(ACTHDR(JJ)))
                     KK=nrlen(2,jj)
                     LL=KK-NRLEN(2,JJ)+1+mpos
                     DO WHILE(KK.LE.NRLEN(3,JJ).AND.
     &                    LL.LE.arglen.AND.FOUND)
                        IF(FOUND) 
     &                       FOUND=(arg1000(LL:LL).EQ.
     &                       HDR250(JJ)(KK:KK))
                        KK=KK+1
                        LL=KK-NRLEN(2,JJ)+1+mpos
                     ENDDO
                  end if
                  IF (FOUND)THEN
                  else
                     JJ=JJ+1
                  end if
               end do
               if (found) then  ! this is a header, stop
                  if (bdeb)write(*,*)myname,'HDR:',JJ,':'//
     &                 arg1000(mpos:pos)//':'
                  if (opos.eq.mpos) then ! return this header (not the minus)
                     buff1000=arg1000(opos+1:pos-1)
                     pos=pos+1
                     LUNIT(CUNIT)=LUNIT(CUNIT)+1
                     bbdone=.true.
                  else          ! return everything before this header
                     pos=mpos
                     buff1000=arg1000(opos:pos-1)
                     LUNIT(CUNIT)=LUNIT(CUNIT)+1
                     bbdone=.true.
                  end if
               else             ! this is NOT a header, continue search...
                  if (bdeb)write(*,*)myname,'NOT:',JJ,':'//
     &                 arg1000(mpos:pos)//':'
                  pos=pos+1
               end if
C            else                ! end of line encountered, return last body
C               buff1000=arg1000(opos:pos-1)
C               LUNIT(CUNIT)=LUNIT(CUNIT)+1
C               bbdone=.true.
            end if
         else                   ! EOL
            buff1000=arg1000(opos:pos-1)
            LUNIT(CUNIT)=LUNIT(CUNIT)+1
            bbdone=.true.
         end if
C     
C     END OF FILE
C     
         IF (LINE.NE.-1.and.endoff) THEN
            IF (CNT.LT.CODHDR(LINE)
     &           .AND. CODHDR(LINE).NE.-1) THEN
               WRITE(*,*) MYNAME,'EOL interrupts data body.'
               IRC=174
               RETURN
            END IF
         END IF
      end do
C     
      call chop0(buff1000,1000)
      lenb=length(buff1000,1000,1)

C      write(*,*)myname,pos,arglen,'Given:'//arg1000(1:arglen)
      if (bdeb)write(*,*)myname,'Found:'//buff1000(1:lenb)
C
      RETURN
C
      END

      
#__file: 'isint.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDISINT
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCISINT/ LENS
      DATA LENS /1/
      END BLOCK DATA


      LOGICAL FUNCTION ISINT(STRING,OUT)
C     ***********************************************************************
C     +                                                                     *
C     CHECKS IF STRING IS AN INTEGER                                        *
C     +                                                                     *
CI    STRING (C*250) = STRING TO BE EXAMINED                                  *
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
      CHARACTER*250 STRING
      INTEGER OUT
C
      INTEGER LENS,II,LENGTH,NRSG
      LOGICAL BUFF, DIGIT
      EXTERNAL LENGTH,DIGIT
      COMMON /CCISINT/ LENS
C
      CALL CHOP0(STRING,250)
      LENS=LENGTH(STRING,250,LENS)
C
      BUFF=(LENS.GT.0)
      NRSG=0
      II=0
      DO WHILE (II.LT.LENS.AND.BUFF)
         II=II+1
         IF (BUFF) THEN
            IF (STRING(II:II).EQ.'-'.OR.STRING(II:II).EQ.'+') THEN
               NRSG=NRSG+1
               BUFF=(II.EQ.1.AND.NRSG.LE.1)
            ELSE
               BUFF=(DIGIT(STRING(II:II)))
            ENDIF
         ENDIF
      ENDDO
C
      IF (BUFF) THEN
         READ(STRING(1:LENS),*,ERR=99,END=99) OUT
         ISINT=BUFF
         RETURN
 99      BUFF=.FALSE.
         ISINT=BUFF
         RETURN
      ENDIF
C
      ISINT=BUFF
      RETURN
      END
#__file: 'islog.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDISLOG
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCISLOG/ LENS
      DATA LENS /1/
      END BLOCK DATA



      LOGICAL FUNCTION ISLOG(STRING,OUT)
C     ***********************************************************************
C     +                                                                     *
C     CHECKS IF STRING IS LOGICAL                                           *
C     +                                                                     *
CI    STRING (C*250) = STRING TO BE EXAMINED                                  *
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
      CHARACTER*250 STRING
      LOGICAL OUT
C
      CHARACTER*7 TEXT
      INTEGER LENS,LENGTH
      LOGICAL BUFF
      EXTERNAL LENGTH
C
      COMMON /CCISLOG/ LENS
C
      CALL CHOP0(STRING,250)
      LENS=MIN(MAX(LENS,1),250)
      LENS=LENGTH(STRING,250,LENS)
C
      BUFF=.FALSE.
      IF (LENS.LE.7) THEN
C
         CALL RESET(TEXT,7)
         TEXT(1:LENS)=STRING(1:LENS)
C
         IF (TEXT(1:LENS).EQ.'T') THEN
            OUT=.TRUE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'TRUE') THEN
            OUT=.TRUE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'.TRUE.') THEN
            OUT=.TRUE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'F') THEN
            OUT=.FALSE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'FALSE') THEN
            OUT=.FALSE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'.FALSE.') THEN
            OUT=.FALSE.
            BUFF=.TRUE.
         ENDIF
      ENDIF
C
      ISLOG=BUFF
C
      RETURN
      END
#__file: 'isreal.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDISREAL
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCISREAL/ LENS
      DATA LENS /1/
      END BLOCK DATA

      LOGICAL FUNCTION ISREAL(STRING,OUT)
C     ***********************************************************************
C     +                                                                     *
C     CHECKS IS STRING IS A REAL NUMBER                                     *
C     +                                                                     *
CI    STRING (C*250) = STRING TO BE EXAMINED                                  *
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
      CHARACTER*250 STRING
      REAL OUT
C
      INTEGER LENS,II,LENGTH,
     &     POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE
      LOGICAL BUFF, DIGIT
      EXTERNAL DIGIT,LENGTH
      COMMON /CCISREAL/ LENS
C
      character*8 myname
      data myname /'ISREAL'/
C
      logical bdeb
      bdeb=.false.
C
      CALL CHOP0(STRING,250)
      LENS=LENGTH(STRING,250,LENS)
C
C
      POSSG=-1
      POSNR=-1
      POSDE=-1
      POSDD=-1
C
      NRSGN=0
      NRNRR=0
      NRDES=0
      NRDDE=0
C
      BUFF=(LENS.GT.0)
      II=0
      DO WHILE (II.LT.LENS.AND.BUFF)
         II=II+1
         IF (BUFF) THEN
            IF (STRING(II:II).EQ.'.') THEN
               NRDES=NRDES+1
               POSDE=II
               BUFF=(NRDES.LE.1.AND.NRDDE.LT.1)
            ELSEIF (STRING(II:II).EQ.'D' .OR. STRING(II:II).EQ.'E') THEN
               NRDDE=NRDDE+1
               POSDD=II
               BUFF=(NRDDE.LE.1 .AND. II.LT.LENS)
            ELSEIF (STRING(II:II).EQ.'-'.OR.STRING(II:II).EQ.'+') THEN
               NRSGN=NRSGN+1
               POSSG=II
               BUFF=((NRSGN.LE.1.AND.II.EQ.1) .OR.
     &              (NRSGN.LE.2.AND.NRDDE.EQ.1
     &              .AND.POSDD.EQ.(POSSG-1)
     &              .AND.II.LT.LENS))
            ELSE
               BUFF=(DIGIT(STRING(II:II)))
               IF (BUFF) POSNR=II
            ENDIF
         ENDIF
      ENDDO
C
      IF (BUFF) BUFF=NRDES.GT.0
C
      IF (BUFF) THEN
         READ(STRING(1:LENS),*,ERR=99,END=99) OUT
         ISREAL=BUFF
         if (bdeb) write(*,*)myname,'Val:',
     &        string(1:lens),out
         RETURN
 99      BUFF=.FALSE.
         ISREAL=BUFF
         RETURN
      ENDIF
C
      ISREAL=BUFF
      RETURN
      END
#__file: 'item.F' 0100664    **DO NOT DELETE**
      SUBROUTINE ITEM(STRING,N,DEL,LEV,POS,IRC)
C     ***********************************************************************
C     +                                                                     *
C     RETURNS THE POSITION OF THE SUBSTRING (AT LEVEL LEV), WITH THE        *
C     DELIMITERS DEL(2). IF THE SPECIFIED LEVEL IS NOT FOUND, THE ROUTINE   *
C     RETURNS THE LOWEST LEVEL IN THE STRING, E.G. STRING="1+((2+(3)))"     *
C     CALLED WITH DEL(1)='(',DEL(2)=')' AND LEV=-1 GIVES POS(1)=7,POS(2)=9  *
C     +                                                                     *
CI    STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CI    DEL(2) (C*1) = THE START AND END DELIMITER TO SEARCH FOR              *
CI    LEV = LEVEL TO SEARCH FOR, E.G. '(a)' AND '((a))' ARE RESP. LEV=1,2   *
CIO   POS(2) (I*4) = POSITION TO SEARCH WITHIN/LOCATION OF SUBSTRING        *
CO    IRC (I*4) = ERROR RETURN CODE (IRC=0 ALL OK)                          *
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
C     STRING WHOLE STRING IN WHICH TO SEARCH
C     N= LENGTH OF STRING
C     DEL=DELIMITERS IN WHICH TO SEARCH FOR
C     POS=LIMIT OF STRING TO SEARCH FOR, ALSO LOC OF DEL
C
C     IDENTIFIES AN ITEM STARTING WITH DEL(1) AND ENDING WITH DEL(2)
C
      IMPLICIT NONE
C
      INTEGER N,LEV
      CHARACTER*1 DEL(2)
      CHARACTER*(*) STRING
      CHARACTER(len=n) STRING2
      INTEGER II,IRC,POS(2),MPOS(2),LEVEL,MAXLEV,START,STOP
      LOGICAL FOUND(2),MFOUND(2)
C
C     FIND START
C
      STRING2=STRING
      START=POS(1)
      STOP=POS(2)
C
      LEVEL=0
      MAXLEV=-1
      MPOS(1)=START
      MPOS(2)=STOP
      POS(1)=0
      POS(2)=0
      FOUND(1)=.FALSE.
      FOUND(2)=.FALSE.
C
      II=START
      DO WHILE (II.LE.STOP)
         IF (STRING2(II:II).EQ.DEL(1)) THEN
            IF (.NOT. FOUND(1) .AND. LEVEL.EQ.LEV) THEN
               POS(1)=II
               FOUND(1)=.TRUE.
            ENDIF
            LEVEL=LEVEL+1
            IF (LEVEL.GT.MAXLEV) THEN
               MAXLEV=LEVEL
               MPOS(1)=II
               MFOUND(1)=.TRUE.
               MFOUND(2)=.FALSE.
            ENDIF
         ELSEIF(STRING2(II:II).EQ.DEL(2)) THEN
            IF (.NOT.MFOUND(2).AND.LEVEL.EQ.MAXLEV) THEN
               MPOS(2)=II
               MFOUND(2)=.TRUE.
            ENDIF
            LEVEL=LEVEL-1
            IF (.NOT. FOUND(2) .AND. LEVEL.EQ.LEV) THEN
               POS(2)=II
               FOUND(2)=.TRUE.
            ENDIF
         ENDIF
         IF (LEVEL.LT.0) THEN
            IRC=98
         ENDIF
         II=II+1
      ENDDO
C
      IF (LEVEL.NE.0) IRC=99
C
      IF (.NOT. FOUND(1) .OR. .NOT. FOUND(2)) THEN
         LEV=MAXLEV
         POS(1)=MPOS(1)
         POS(2)=MPOS(2)
      ENDIF
C
      string=string2
      RETURN
      END
#__file: 'jd2000.F' 0100664    **DO NOT DELETE**
      SUBROUTINE JD2000(DAY,YY,MM,DD,HH,MI,SEC)
CP GIVES THE NEW MOD. JULIAN DAY (MJD=0.0 ON 2000/JAN/1 AT 0:00:00)
CP FOR INPUT CALENDAR DATES BETWEEN 1950/JAN/1 AND 2099/DEC/31.
C
C   MJD(2000) = MJD(1950) - 18262.0 IS = 0 ON 2000/01/01 AT 00:00:00.
C
CI  (INT*4) YY = YEAR WITH 2 OR 4 DIGITS; 2 DIGITS => 1950 TO 2049
CI  (INT*4) MM = MONTH
CI  (INT*4) DD = DAY
CI  (INT*4) HH = HOUR
CI  (INT*4) MI = MINUTE
CI  (REAL) SEC = SECOND.
CO  (REAL) DAY = MOD. JUL. DAY, REFERRED TO 2000.
C
      IMPLICIT NONE
C
      CHARACTER*40 SCCS_INFO
      DATA SCCS_INFO
     & /'@(#)jd2000.f \n'/
C
      REAL DAY,SEC
      INTEGER YY, MM,DD,HH,MI,JJ,L
      JJ = (14 - MM)/12
      L = YY - JJ - 1900*(YY/1900) + 100*(2000/(YY+1951))
      DAY = DD-36496+(1461*L)/4+(367*(MM-2+JJ*12))/12
      DAY = DAY + (DFLOAT((HH*60 + MI)*60) + SEC)/864.D2
      RETURN
      END
#__file: 'length.F' 0100664    **DO NOT DELETE**
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
#__file: 'nukecp.F' 0100664    **DO NOT DELETE**
      SUBROUTINE NUKECP(STRING,ST1,ST2,N)
C     ***********************************************************************
C     +                                                                     *
C     REPLACES ST1 BY ST2 IN STRING (WITH LENGTH N)
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    ST1 (C*1) = CHARACTER(S) WHICH SHOULD BE REMOVED FROM STRING          *
CI    ST2 (C*1) = CHARACTER WHICH SHOULD REPLACE ST1                        *
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
      CHARACTER*1 ST1,ST2
      INTEGER II,LENS,LENGTH
      EXTERNAL LENGTH
C
      LENS=LENGTH(STRING,N,1)
C
      DO II=1,LENS
         IF (STRING(II:II).EQ.ST1) STRING(II:II)=ST2
      ENDDO
C
      CALL CHOP0(STRING,LENS)
C
      RETURN
      END
#__file: 'nuke.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDNUKE
      IMPLICIT NONE
      LOGICAL INITIALISED,ACTIVE,BDEB
C
      INTEGER MAXHDR
      PARAMETER (MAXHDR=100)
C
      LOGICAL LFLDAT(MAXHDR) ! 
      INTEGER NROPT, MAXGRP
      PARAMETER (NROPT=5,MAXGRP=250)
      LOGICAL PPDAT(NROPT,MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR),CODHDR(MAXHDR),NRHDR,CURHDR
      INTEGER HDRGRP(6,MAXGRP),NRGRP
      LOGICAL LFLGRP(MAXGRP)
      LOGICAL IENDOFF
C
      INTEGER MAXCOM,NRCOM,
     &     LENGTH,LENB,LENF,MEAN
      PARAMETER (MAXCOM=13)
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      CHARACTER*250 NUKEHEAD
      CHARACTER*1000 BUFF1000,PATHF
C      INTEGER CMLEN(MAXCOM),system,putenv,FTUNIT
C      EXTERNAL system,putenv
      INTEGER CMLEN(MAXCOM),FTUNIT
C
      INTEGER    LINE,CNT
C
      LOGICAL BDONE,PROCESS
C
C     AUXILIARY VARIABLES
C
      INTEGER MAXVAR,IFLVL
      PARAMETER (MAXVAR=50)
      INTEGER VARLEN(MAXVAR),NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
C
C     UNIT CONTROLLERS
C
      INTEGER MAXUNIT
      PARAMETER (MAXUNIT=10)
      INTEGER UNITA(MAXUNIT),CUNIT,LUNIT(MAXUNIT),II
      CHARACTER*250 FILES(MAXUNIT)
C
      COMMON /CCNUKE/INITIALISED,ACTIVE,BDEB,
     &     LFLDAT,PPDAT,LFLGRP,ACTHDR,NRLEN,CODHDR,NRHDR,CURHDR,
     &     HDRGRP,NRGRP,IENDOFF,LENF,LENB,COM20,HLP20,BUFF1000,PATHF,
     &     CMLEN,LINE,CNT,BDONE,PROCESS,IFLVL,VARLEN,NRVAR,NAMVAR,
     &     VALVAR,UNITA,CUNIT,LUNIT,FILES
C
      DATA INITIALISED/.false./, ACTIVE /.FALSE./
      END BLOCK DATA

      SUBROUTINE NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,
     &     ENDOFF,IRC)
C     ***********************************************************************
C     +                                                                     *
C     INTERFACE SUBROUTINE TO FORTRAN-PROGRAM INPUT FILES                   *
C     +                                                                     *
CI    KODE (I*4) = INITIALISATION CODE,                                     *
C     +     KODE=-1 REINITIALISES                                           *
C     +     KODE=0 READ A DATA-BODY ITEM                                    *
C     +     KODE=-1 TERMINATE READING                                       *
CI    UNITI (I*4) = UNIT NUMBER FOR INPUT FILE (5=STANDARD INPUT)           *
CI    FILENM (C*250) = FILE NAME
CI    HDR250(250) (C*250) = HEADERS TO SEARCH FOR IN INPUT FILE             *
C     +   HEADER MUST BE ON FORM 'THIS IS THE HEADER [N]P', WHERE N         *
C     +   IS THE NUMBER OF LINES INCLUDED IN DATA LINE (CONCATENATED).      *
C     +   IF 'N'='*' THEN ONE DATA LINE IS RETURNED PER LINE IN THE FILE    *
C     +   (NOTE: THE HEADER IS IN THIS CASE ONLY NEEDED ONCE).              *
C     +   A 'P' AT THE END IMPLIES THAT THE DATA SHOULD BE PREPROCESSED.    *
CO    INTOUT (I*4) = HEADER ID (LOC IN ARRAY) FOR DATA FOUND IN FILE *
CO    DAT250 (C*250) = DATA BODY                                     *
C     +      II=INTOUT(JJ) => HEADER HDR250(II) HAD DATABODY DAT250(JJ)     *
CIO   IRC (I*4) = ERROR RETURN CODE (IRC=0, ALL OK)                         *
C     +     A CALL WITH (IRC.NE.0) WILL INITIALISE THE ROUTINE              *
C     +                                                                     *
C     NOTE :                                                                *
C     +   o THE LINE '?' IN INPUT FILE WILL LIST ALL HEADERS AND COMMANDS   *
C     +   o IT IS POSSIBLE TO BYPASS THE MATHEMATICAL PREPROCESSOR          *
C     +     (IF IT FAILS TO WORK). IF THE MATHEMATICAL EXPRESSION IS        *
C     +     CONTAINED IN {}, IT WILL NOT BE EVALUATED ({} ARE DELETED), AN  *
C     +     ALTERNATIVE IS [] ([] ARE CONVERTED INTO ()).                   *
C     +                                                                     *
C     EXAMPLE OF CALL SEQUENCE :                                            *
C     +                                                                     *
C     INTEGER IRC,KODE,UNITI                                                *
C     CHARACTER*250 HDR250(250),DAT250                                      *
C     INTEGER INTOUT                                                        *
C     LOGICAL ENDOFF                                                        *
C     UNITI=5                                                               *
C     HDR250(1)='THIS IS A HEADER : [*]P'                                   *
C     KODE=-1                                                               *
C     CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,ENDOFF,IRC)          *
C     KODE=0                                                                *
C     CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,ENDOFF,IRC)          *
C     KODE=+1                                                               *
C     CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,ENDOFF,IRC)          *
C     +                                                                     *
C     EXAMPLE OF INPUT FILE (THE 'C     ' AND '*' SHOULD BE IGNORED)        *
C     +                                                                     *
C     # first line (comment)                                                *
C     # test input file (yet another comment)                               *
C       THIS IS A HEADER :                                                  *
C        data-string data1                                                  *
C        1+2 +1.0D-4^-2 +23/1.01D2 +(1*(-1*(1*(-1*(1*(-2)))))) data2        *
C     # (...and another comment) The next line gives (some) help            *
C        ?                                                                  *
C     # last line (last comment)                                            *
C     +                                                                     *
C     THE DATA IN THE ABOVE FILE IS INTERPRETED AS :                        *
C     data-string data1                                                     *
C     3 +100000000. +0.22772277227723 +(-2) data2                           *
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
      INTEGER MAXHDR
      PARAMETER (MAXHDR=100)
C
C     INTERFACE VARIABLES
C
      INTEGER KODE,UNITI
      CHARACTER*250 FILENM,HDR250(MAXHDR),DAT250
      INTEGER INTOUT,IRC
      LOGICAL ENDOFF
C
C     INTERNAL VARIABLES
C
      LOGICAL INITIALISED,ACTIVE,BDEB
      LOGICAL LFLDAT(MAXHDR) ! 
      INTEGER NROPT, MAXGRP
      PARAMETER (NROPT=5,MAXGRP=250)
      LOGICAL PPDAT(NROPT,MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR),CODHDR(MAXHDR),NRHDR,CURHDR
      INTEGER HDRGRP(6,MAXGRP),NRGRP
      LOGICAL LFLGRP(MAXGRP)
      LOGICAL IENDOFF
C
      INTEGER MAXCOM,NRCOM,
     &     LENGTH,LENB,LENF,MEAN
      PARAMETER (MAXCOM=13)
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      CHARACTER*250 NUKEHEAD
      CHARACTER*1000 BUFF1000,PATHF
      EXTERNAL FTUNIT,LENGTH
C      INTEGER CMLEN(MAXCOM),system,putenv,FTUNIT
C      EXTERNAL system,putenv
      INTEGER CMLEN(MAXCOM),FTUNIT
C
      INTEGER    LINE,CNT
C
      LOGICAL BDONE,PROCESS
C
C     AUXILIARY VARIABLES
C
      INTEGER MAXVAR,IFLVL
      PARAMETER (MAXVAR=50)
      INTEGER VARLEN(MAXVAR),NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
C
C     UNIT CONTROLLERS
C
      INTEGER MAXUNIT
      PARAMETER (MAXUNIT=10)
      INTEGER UNITA(MAXUNIT),CUNIT,LUNIT(MAXUNIT),II
      CHARACTER*250 FILES(MAXUNIT)
C
      COMMON /CCNUKE/INITIALISED,ACTIVE,BDEB,
     &     LFLDAT,PPDAT,LFLGRP,ACTHDR,NRLEN,CODHDR,NRHDR,CURHDR,
     &     HDRGRP,NRGRP,IENDOFF,LENF,LENB,COM20,HLP20,BUFF1000,PATHF,
     &     CMLEN,LINE,CNT,BDONE,PROCESS,IFLVL,VARLEN,NRVAR,NAMVAR,
     &     VALVAR,UNITA,CUNIT,LUNIT,FILES
C
C     EQUAL SIGN IDENTIFIERS
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'NUKE'/
C
C     Debug System.
C
      BDEB=.false.
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C
      INTOUT=0
C
      IF (KODE.EQ.-1) THEN
         IF (INITIALISED) THEN
            WRITE(*,*) MYNAME,'ROUTINE IS ALLREADY INITIALISED'
            IRC=42
            RETURN
         END IF
C
C     INITIALISE
C
         LINE=-1
         ENDOFF=.FALSE.
         NRGRP=0
         IFLVL=0
         LFLGRP(:)=.FALSE.
C
C     C1=CHAR(0)
C
C     DEFINE COMMANDS
C
         CALL GETCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from GETCOM.',IRC
            RETURN
         END IF
C     
C     ANALYZE HEADERS
C
         CALL ANAHDR(MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &        CODHDR,PPDAT,MAXGRP,NRGRP,HDRGRP,
     &        IRC)
C
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from ANAHDR.',IRC
            RETURN
         END IF
C
         DO II=1,NRHDR
            LFLDAT(II)=.FALSE.
         END DO
C
C     GET FIRST LINE
C
         CNT=0
         CUNIT=1
         UNITA(CUNIT)=UNITI
         FILES(CUNIT)=FILENM
         CALL CHOP0(FILES(CUNIT),250)
         LENF=LENGTH(FILES(CUNIT),250,10)
C
         IENDOFF=.FALSE.
         CALL GETLINE(KODE,MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &        CNT,MAXHDR,CODHDR,LINE,
     &        BUFF1000,LENB,IENDOFF,IRC)
C
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from GETLINE.',IRC
            RETURN
         END IF
C
         IF (IENDOFF) THEN
            IRC=266
            WRITE(*,*) MYNAME,
     &           'Use the "?" command in the input file '//
     &           'to list the possible headers.'
            WRITE(*,'(A)') '#'
            WRITE(*,'(A)') ' ?'
            WRITE(*,'(A)') '#'
            WRITE(*,*) MYNAME,'Input file is empty:'//FILENM(1:LENF)
            RETURN
         END IF
C
         INITIALISED = .TRUE.
C
      ELSE IF (KODE.EQ.0) THEN  ! analyse next line
C
         IF (.NOT.INITIALISED) THEN
            WRITE(*,*) MYNAME,'ROUTINE NEEDS TO BE INITIALISED'
            IRC=44
            RETURN
         END IF
C
         INTOUT=0
         DAT250=''
C
         BDONE=IENDOFF
         DO WHILE (.NOT.BDONE)
C
            PROCESS=.TRUE.
C
C     CHECK FOR COMMAND
C
            CALL QCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,PPDAT,
     &           MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &           MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &           MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,IFLVL,
     &           MAXGRP,NRGRP,HDRGRP,LFLGRP,
     &           BUFF1000,LENB,PROCESS,BDONE,IENDOFF,IRC)
C
            IF (IRC.NE.0) THEN
               WRITE(*,*) MYNAME,'Error return from QCOM.',
     &              IRC
               RETURN
            END IF
C     
C     CHECK IF HAVE A HEADER
C
            IF (PROCESS) THEN
               CALL QHDR(MAXUNIT,FILES,LUNIT,CUNIT,
     &              MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &              LFLDAT,INTOUT,CNT,CODHDR,LINE,
     &              BUFF1000,LENB,PROCESS,BDONE,IRC)
C
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from QHDR.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
C     CHECK IF WE SHOULD STORE AS A DATA-BODY
C
            IF (PROCESS) THEN
               CALL QBDY(MAXUNIT,FILES,LUNIT,CUNIT,
     &              MAXHDR,HDR250,NRLEN,PPDAT,INTOUT,
     &              CNT,CODHDR,LINE,DAT250,
     &              MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,
     &              BUFF1000,LENB,PROCESS,BDONE,IRC)
C
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from QBDY.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
C     WRITE ERROR MESSAGE IF WE WERE NOT ABLE TO PROCESS LINE
C
            IF (PROCESS) THEN
               
               WRITE(*,*) MYNAME,
     &              '"'//FILES(CUNIT)(1:LENF)//'", line ',
     &              LUNIT(CUNIT),': Error: Unable to interpret:',
     &              BUFF1000(1:LENB)
               IRC=999
               RETURN
            END IF
C
C     GET NEXT LINE IF NOT EOF
C
            IF (.NOT.IENDOFF) THEN ! we may have encountered a "stop" statement
               BUFF1000=''
               CALL GETLINE(KODE,MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &              CNT,MAXHDR,CODHDR,LINE,
     &              BUFF1000,LENB,IENDOFF,IRC)
C
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from GETLINE.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
            IF (IENDOFF) THEN   ! ok, wrap it up...
               BDONE=.TRUE.
               IF (LINE.NE.-1) THEN
                  IF (CNT.EQ.0 .AND. CODHDR(LINE).EQ.-1) THEN
                     LINE=-1
                     INTOUT=0
                  END IF
               ELSE
                  INTOUT=0
               END IF
            END IF
C     
         END DO
C
C     REPORT EOF IF WE ARE RETURNING DATA...
C
         ENDOFF=IENDOFF
C
         IF (INTOUT.NE.0) THEN
            LFLDAT(INTOUT)=.TRUE.
         END IF
C
      ELSE IF (KODE.EQ.1) THEN
         INITIALISED =.FALSE.
C     
C     CHECK THAT READING TERMINATED CORRECTLY
C     
C     
C     CHECK FOR EOF INSIDE FIXED FORMAT DATA-BODY...
C     
         IF (LINE.NE.-1) THEN
            IF (CODHDR(LINE).GT.0 .AND. CNT.NE.CODHDR(LINE)) THEN
               WRITE(*,*) MYNAME,
     &              'End of file interrupts data body.'
               IRC=345
               RETURN
            END IF
         END IF
C     
C     CHECK FOR NESTING ERROR IN IF-EXPRESSIONS...
C     
         IF (IFLVL.NE.0) THEN
            WRITE(*,*) MYNAME,
     &           'End of file interrupts if test.'
            IRC=345
            RETURN
         END IF
C     
C     CHECK FOR MISSING HEADERS...
C     
         CALL ANALFL(MAXHDR,NRHDR,HDR250,
     &        ACTHDR,LFLDAT,NRLEN,
     &        MAXGRP,NRGRP,HDRGRP,
     &        IRC)
         IF (IRC.EQ.391.AND.FILES(CUNIT)(1:11).EQ.'mkbccfinput') THEN
            IRC=0
            WRITE(*,*) MYNAME,' IRC reset from 391'
            WRITE(*,*) ' CHECK if problem is merely redundant headers'
         ENDIF
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from ANALFL.',
     &           IRC
            RETURN
         END IF
C
C     RELASE THE UNITI UNIT NUMBER
C
         IF (UNITI.GT.10) THEN
            CLOSE(UNITI,IOSTAT=IRC)
            IF (IRC.NE.0) IRC=0
         END IF
C     
      END IF
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine ends.',IRC
C
      RETURN
      END
#__file: 'nukehead.F' 0100664    **DO NOT DELETE**
      CHARACTER*250 FUNCTION NUKEHEAD(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     FUNCTION RETURNS THE FIRST ITEM IN STRING,                            *
C     AND REMOVES IT FROM STRING                                            *
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CO    NUKEHEAD (C*250) = HEAD OF STRING                                     *
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
      LOGICAL BLNK,FIRST
      DATA FIRST /.TRUE./
      CHARACTER*16 MYNAME
      DATA MYNAME /'NUKEHEAD'/
C
      C1=CHAR(0)
C
C     CRR = CURRENT POINTER POSITION IN STRING
      CRR=0
C
      BLNK=.FALSE.
C
      DO WHILE (.NOT.BLNK .AND. CRR.LT.N)
         CRR = CRR+1
         BLNK= (STRING(CRR:CRR) .EQ. ' ')
      ENDDO
C
      CALL RESET(OUT,250)
      IF (BLNK) THEN
C     SEVERAL ITEMS IN STRING
         JJ=MIN(250,CRR)
         DO II=1,JJ
            OUT(II:II)=STRING(II:II)
         ENDDO
         JJ=N-CRR
         DO II=1,JJ
            STRING(II:II)=STRING(II+CRR:II+CRR)
         ENDDO
         DO II=JJ+1,N
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
      NUKEHEAD=OUT
      RETURN
      END
#__file: 'nukem.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDNUKEM
      IMPLICIT NONE
      LOGICAL INITIALISED,ACTIVE,BDEB
C
      INTEGER MAXHDR
      PARAMETER (MAXHDR=100)
C
      LOGICAL LFLDAT(MAXHDR) ! 
      INTEGER NROPT, MAXGRP
      PARAMETER (NROPT=5,MAXGRP=250)
      LOGICAL PPDAT(NROPT,MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR),CODHDR(MAXHDR),NRHDR,CURHDR
      INTEGER HDRGRP(6,MAXGRP),NRGRP
      LOGICAL LFLGRP(MAXGRP)
      LOGICAL IENDOFF
C
      INTEGER MAXCOM,NRCOM,
     &     LENGTH,LENB,LENF,MEAN
      PARAMETER (MAXCOM=13)
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      CHARACTER*250 NUKEHEAD
      CHARACTER*1000 BUFF1000,PATHF
C      INTEGER CMLEN(MAXCOM),system,putenv,FTUNIT
C      EXTERNAL system,putenv
      INTEGER CMLEN(MAXCOM),FTUNIT
C
      INTEGER    LINE,CNT
C
      LOGICAL BDONE,PROCESS
C
C     AUXILIARY VARIABLES
C
      INTEGER MAXVAR,IFLVL
      PARAMETER (MAXVAR=50)
      INTEGER VARLEN(MAXVAR),NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
C
C     UNIT CONTROLLERS
C
      INTEGER MAXUNIT
      PARAMETER (MAXUNIT=10)
      INTEGER UNITA(MAXUNIT),CUNIT,LUNIT(MAXUNIT),II
      CHARACTER*250 FILES(MAXUNIT)
C
      COMMON /CCNUKEM/INITIALISED,ACTIVE,BDEB,
     &     LFLDAT,PPDAT,LFLGRP,ACTHDR,NRLEN,CODHDR,NRHDR,CURHDR,
     &     HDRGRP,NRGRP,IENDOFF,LENF,LENB,COM20,HLP20,BUFF1000,PATHF,
     &     CMLEN,LINE,CNT,BDONE,PROCESS,IFLVL,VARLEN,NRVAR,NAMVAR,
     &     VALVAR,UNITA,CUNIT,LUNIT,FILES
C
      DATA INITIALISED/.false./, ACTIVE /.FALSE./
      END BLOCK DATA

      SUBROUTINE NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,
     &     ENDOFF,IRC)
C     ***********************************************************************
C     +                                                                     *
C     INTERFACE SUBROUTINE TO FORTRAN-PROGRAM INPUT FILES                   *
C     +                                                                     *
CI    KODE (I*4) = INITIALISATION CODE,                                     *
C     +     KODE=-1 REINITIALISES                                           *
C     +     KODE=0 READ A DATA-BODY ITEM                                    *
C     +     KODE=-1 TERMINATE READING                                       *
CI    UNITI (I*4) = UNIT NUMBER FOR INPUT FILE (5=STANDARD INPUT)           *
CI    HDR250(250) (C*250) = HEADERS TO SEARCH FOR IN INPUT FILE             *
C     +   HEADER MUST BE ON FORM 'THIS IS THE HEADER [N]P', WHERE N         *
C     +   IS THE NUMBER OF LINES INCLUDED IN DATA LINE (CONCATENATED).      *
C     +   IF 'N'='*' THEN ONE DATA LINE IS RETURNED PER LINE IN THE FILE    *
C     +   (NOTE: THE HEADER IS IN THIS CASE ONLY NEEDED ONCE).              *
C     +   A 'P' AT THE END IMPLIES THAT THE DATA SHOULD BE PREPROCESSED.    *
CO    INTOUT (I*4) = HEADER ID (LOC IN ARRAY) FOR DATA FOUND IN FILE *
CO    DAT250 (C*250) = DATA BODY                                     *
C     +      II=INTOUT(JJ) => HEADER HDR250(II) HAD DATABODY DAT250(JJ)     *
CIO   IRC (I*4) = ERROR RETURN CODE (IRC=0, ALL OK)                         *
C     +     A CALL WITH (IRC.NE.0) WILL INITIALISE THE ROUTINE              *
C     +                                                                     *
C     NOTE :                                                                *
C     +   o THE LINE '?' IN INPUT FILE WILL LIST ALL HEADERS AND COMMANDS   *
C     +   o IT IS POSSIBLE TO BYPASS THE MATHEMATICAL PREPROCESSOR          *
C     +     (IF IT FAILS TO WORK). IF THE MATHEMATICAL EXPRESSION IS        *
C     +     CONTAINED IN {}, IT WILL NOT BE EVALUATED ({} ARE DELETED), AN  *
C     +     ALTERNATIVE IS [] ([] ARE CONVERTED INTO ()).                   *
C     +                                                                     *
C     EXAMPLE OF CALL SEQUENCE :                                            *
C     +                                                                     *
C     INTEGER IRC,KODE,UNITI                                              *
C     CHARACTER*250 HDR250(250),DAT250                                      *
C     INTEGER INTOUT                                                      *
C     LOGICAL ENDOFF                                                      *
C     UNITI=5                                                               *
C     HDR250(1)='THIS IS A HEADER : [*]P'                                   *
C     KODE=-1                                                               *
C     CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)                *
C     KODE=0                                                                *
C     CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)                *
C     KODE=+1                                                               *
C     CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)                *
C     +                                                                     *
C     EXAMPLE OF INPUT FILE (THE 'C     ' AND '*' SHOULD BE IGNORED)        *
C     +                                                                     *
C     # first line (comment)                                                *
C     # test input file (yet another comment)                               *
C       THIS IS A HEADER :                                                  *
C        data-string data1                                                  *
C        1+2 +1.0D-4^-2 +23/1.01D2 +(1*(-1*(1*(-1*(1*(-2)))))) data2        *
C     # (...and another comment) The next line gives (some) help            *
C        ?                                                                  *
C     # last line (last comment)                                            *
C     +                                                                     *
C     THE DATA IN THE ABOVE FILE IS INTERPRETED AS :                        *
C     data-string data1                                                     *
C     3 +100000000. +0.22772277227723 +(-2) data2                           *
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
      INTEGER MAXHDR
      PARAMETER (MAXHDR=100)
C
C     INTERFACE VARIABLES
C
      INTEGER KODE,UNITI
      CHARACTER*250 HDR250(MAXHDR),DAT250
      INTEGER INTOUT,IRC
      LOGICAL ENDOFF
C
C     INTERNAL VARIABLES
C
      LOGICAL INITIALISED,ACTIVE,BDEB
      LOGICAL LFLDAT(MAXHDR) ! 
      INTEGER NROPT, MAXGRP
      PARAMETER (NROPT=5,MAXGRP=250)
      LOGICAL PPDAT(NROPT,MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR),CODHDR(MAXHDR),NRHDR,CURHDR
      INTEGER HDRGRP(6,MAXGRP),NRGRP
      LOGICAL LFLGRP(MAXGRP)
      LOGICAL IENDOFF
C
      INTEGER MAXCOM,NRCOM,
     &     LENGTH,LENB,LENF,MEAN
      PARAMETER (MAXCOM=13)
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      CHARACTER*250 NUKEHEAD
      CHARACTER*1000 BUFF1000,PATHF
      EXTERNAL FTUNIT,LENGTH
C      INTEGER CMLEN(MAXCOM),system,putenv,FTUNIT
C      EXTERNAL system,putenv
      INTEGER CMLEN(MAXCOM),FTUNIT
C
      INTEGER    LINE,CNT
C
      LOGICAL BDONE,PROCESS
C
C     AUXILIARY VARIABLES
C
      INTEGER MAXVAR,IFLVL
      PARAMETER (MAXVAR=50)
      INTEGER VARLEN(MAXVAR),NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
C
C     UNIT CONTROLLERS
C
      INTEGER MAXUNIT
      PARAMETER (MAXUNIT=10)
      INTEGER UNITA(MAXUNIT),CUNIT,LUNIT(MAXUNIT),II
      CHARACTER*250 FILES(MAXUNIT)
C
      COMMON /CCNUKEM/INITIALISED,ACTIVE,BDEB,
     &     LFLDAT,PPDAT,LFLGRP,ACTHDR,NRLEN,CODHDR,NRHDR,CURHDR,
     &     HDRGRP,NRGRP,IENDOFF,LENF,LENB,COM20,HLP20,BUFF1000,PATHF,
     &     CMLEN,LINE,CNT,BDONE,PROCESS,IFLVL,VARLEN,NRVAR,NAMVAR,
     &     VALVAR,UNITA,CUNIT,LUNIT,FILES
C
C     EQUAL SIGN IDENTIFIERS
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'NUKEM'/
C
C     Debug System.
C
      BDEB=.false.
C      BDEB=.true.
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C
      INTOUT=0
C
      IF (KODE.EQ.-1) THEN
         IF (INITIALISED) THEN
            WRITE(*,*) MYNAME,'ROUTINE IS ALLREADY INITIALISED'
            IRC=42
            RETURN
         END IF
C
C     INITIALISE
C
         LINE=-1
         ENDOFF=.FALSE.
         NRGRP=0
         IFLVL=0
         LFLGRP(:)=.FALSE.
C
C     C1=CHAR(0)
C
C     DEFINE COMMANDS
C
         CALL GETCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from GETCOM.',IRC
            RETURN
         END IF
C     
C     ANALYZE HEADERS
C
         CALL ANAHDR(MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &        CODHDR,PPDAT,MAXGRP,NRGRP,HDRGRP,
     &        IRC)
C
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from ANAHDR.',IRC
            RETURN
         END IF
C
         DO II=1,NRHDR
            LFLDAT(II)=.FALSE.
         END DO
C
C     GET FIRST LINE
C
         CNT=0
         CUNIT=1
         UNITA(CUNIT)=UNITI
         FILES(CUNIT)='input file'
         CALL CHOP0(FILES(CUNIT),250)
         LENF=LENGTH(FILES(CUNIT),250,10)
C
         IENDOFF=.FALSE.
         CALL GETLINE(KODE,MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &        CNT,MAXHDR,CODHDR,LINE,
     &        BUFF1000,LENB,IENDOFF,IRC)
C
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from GETLINE.',IRC
            RETURN
         END IF
C
         IF (IENDOFF) THEN
            IRC=266
            WRITE(*,*) MYNAME,
     &           'Use the "?" command in the input file '//
     &           'to list the possible headers.'
            WRITE(*,'(A)') '#'
            WRITE(*,'(A)') ' ?'
            WRITE(*,'(A)') '#'
            WRITE(*,*) MYNAME,'Input file is empty.',IRC
            RETURN
         END IF
C
         INITIALISED = .TRUE.
C
      ELSE IF (KODE.EQ.0) THEN  ! analyse next line
C
         IF (.NOT.INITIALISED) THEN
            WRITE(*,*) MYNAME,'ROUTINE NEEDS TO BE INITIALISED'
            IRC=44
            RETURN
         END IF
C
         INTOUT=0
         DAT250=''
C
         BDONE=IENDOFF
         DO WHILE (.NOT.BDONE)
C
            PROCESS=.TRUE.
C
C     CHECK FOR COMMAND
C
            CALL QCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,PPDAT,
     &           MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &           MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &           MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,IFLVL,
     &           MAXGRP,NRGRP,HDRGRP,LFLGRP,
     &           BUFF1000,LENB,PROCESS,BDONE,IENDOFF,IRC)
C
            IF (IRC.NE.0) THEN
               WRITE(*,*) MYNAME,'Error return from QCOM.',
     &              IRC
               RETURN
            END IF
C     
C     CHECK IF HAVE A HEADER
C
            IF (PROCESS) THEN
               CALL QHDR(MAXUNIT,FILES,LUNIT,CUNIT,
     &              MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &              LFLDAT,INTOUT,CNT,CODHDR,LINE,
     &              BUFF1000,LENB,PROCESS,BDONE,IRC)
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from QHDR.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
C     CHECK IF WE SHOULD STORE AS A DATA-BODY
C
            IF (PROCESS) THEN
               CALL QBDY(MAXUNIT,FILES,LUNIT,CUNIT,
     &              MAXHDR,HDR250,NRLEN,PPDAT,INTOUT,
     &              CNT,CODHDR,LINE,DAT250,
     &              MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,
     &              BUFF1000,LENB,PROCESS,BDONE,IRC)
C
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from QBDY.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
C     WRITE ERROR MESSAGE IF WE WERE NOT ABLE TO PROCESS LINE
C
            IF (PROCESS) THEN
               
               WRITE(*,*) MYNAME,
     &              '"'//FILES(CUNIT)(1:LENF)//'", line ',
     &              LUNIT(CUNIT),': Error: Unable to interpret:',
     &              BUFF1000(1:LENB)
               IRC=999
               RETURN
            END IF
C
C     GET NEXT LINE IF NOT EOF
C
            IF (.NOT.IENDOFF) THEN ! we may have encountered a "stop" statement
               BUFF1000=''
               CALL GETLINE(KODE,MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &              CNT,MAXHDR,CODHDR,LINE,
     &              BUFF1000,LENB,IENDOFF,IRC)
C
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from GETLINE.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
            IF (IENDOFF) THEN   ! ok, wrap it up...
               BDONE=.TRUE.
               IF (LINE.NE.-1) THEN
                  IF (CNT.EQ.0 .AND. CODHDR(LINE).EQ.-1) THEN
                     LINE=-1
                     INTOUT=0
                  END IF
               ELSE
                  INTOUT=0
               END IF
            END IF
C     
         END DO
C
C     REPORT EOF IF WE ARE RETURNING DATA...
C
         ENDOFF=IENDOFF
C
         IF (INTOUT.NE.0) THEN
            LFLDAT(INTOUT)=.TRUE.
         END IF
C
      ELSE IF (KODE.EQ.1) THEN
         INITIALISED =.FALSE.
C     
C     CHECK THAT READING TERMINATED CORRECTLY
C     
C     
C     CHECK FOR EOF INSIDE FIXED FORMAT DATA-BODY...
C     
         IF (LINE.NE.-1) THEN
            IF (CODHDR(LINE).GT.0 .AND. CNT.NE.CODHDR(LINE)) THEN
               WRITE(*,*) MYNAME,
     &              'End of file interrupts data body.'
               IRC=345
               RETURN
            END IF
         END IF
C     
C     CHECK FOR NESTING ERROR IN IF-EXPRESSIONS...
C     
         IF (IFLVL.NE.0) THEN
            WRITE(*,*) MYNAME,
     &           'End of file interrupts if test.'
            IRC=345
            RETURN
         END IF
C     
C     CHECK FOR MISSING HEADERS...
C     
         CALL ANALFL(MAXHDR,NRHDR,HDR250,
     &        ACTHDR,LFLDAT,NRLEN,
     &        MAXGRP,NRGRP,HDRGRP,
     &        IRC)
         IF (IRC.EQ.391.AND.FILES(CUNIT)(1:11).EQ.'mkbccfinput') THEN
            IRC=0
            WRITE(*,*) MYNAME,' IRC reset from 391'
            WRITE(*,*) ' CHECK if problem is merely redundant headers'
         ENDIF
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from ANALFL.',
     &           IRC
            RETURN
         END IF
C
C     RELASE THE UNITI UNIT NUMBER
C
         IF (UNITI.GT.10) THEN
            CLOSE(UNITI,IOSTAT=IRC)
            IF (IRC.NE.0) IRC=0
         END IF
C     
      END IF
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine ends.',IRC
C
      RETURN
      END
#__file: 'nukes.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDNUKES
      IMPLICIT NONE
      LOGICAL INITIALISED,ACTIVE,BDEB
C
      INTEGER MAXHDR
      PARAMETER (MAXHDR=100)
C
      LOGICAL LFLDAT(MAXHDR) ! 
      INTEGER NROPT, MAXGRP
      PARAMETER (NROPT=5,MAXGRP=250)
      LOGICAL PPDAT(NROPT,MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR),CODHDR(MAXHDR),NRHDR,CURHDR
      INTEGER HDRGRP(6,MAXGRP),NRGRP
      LOGICAL LFLGRP(MAXGRP)
      LOGICAL IENDOFF
C
      INTEGER MAXCOM,NRCOM,
     &     LENGTH,LENB,LENF,MEAN
      PARAMETER (MAXCOM=13)
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      CHARACTER*250 NUKEHEAD
      CHARACTER*1000 BUFF1000,PATHF
C      INTEGER CMLEN(MAXCOM),system,putenv,FTUNIT
C      EXTERNAL system,putenv
      INTEGER CMLEN(MAXCOM),FTUNIT
C
      INTEGER    LINE,CNT
C
      LOGICAL BDONE,PROCESS
C
C     AUXILIARY VARIABLES
C
      INTEGER MAXVAR,IFLVL,ARGLEN
      PARAMETER (MAXVAR=50)
      INTEGER VARLEN(MAXVAR),NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
C
C     UNIT CONTROLLERS
C
      INTEGER MAXUNIT
      PARAMETER (MAXUNIT=10)
      INTEGER UNITA(MAXUNIT),CUNIT,LUNIT(MAXUNIT),II
      CHARACTER*250 FILES(MAXUNIT)
C
      COMMON /CCNUKES/INITIALISED,ACTIVE,BDEB,
     &     LFLDAT,PPDAT,LFLGRP,ACTHDR,NRLEN,CODHDR,NRHDR,CURHDR,
     &     HDRGRP,NRGRP,IENDOFF,LENF,LENB,COM20,HLP20,BUFF1000,PATHF,
     &     CMLEN,LINE,CNT,BDONE,PROCESS,IFLVL,VARLEN,NRVAR,NAMVAR,
     &     VALVAR,UNITA,CUNIT,LUNIT,FILES,ARGLEN
C
      DATA INITIALISED/.false./, ACTIVE /.FALSE./,ARGLEN /1/
      END BLOCK DATA

      SUBROUTINE NUKES(KODE,ARG1000,HDR250,INTOUT,DAT250,
     &     ENDOFF,IRC)
C     ***********************************************************************
C     +                                                                     *
C     INTERFACE SUBROUTINE TO FORTRAN-PROGRAM INPUT FILES                   *
C     +                                                                     *
CI    KODE (I*4) = INITIALISATION CODE,                                     *
C     +     KODE=-1 REINITIALISES                                           *
C     +     KODE=0 READ A DATA-BODY ITEM                                    *
C     +     KODE=-1 TERMINATE READING                                       *
CI    ARG1000 (C*1000) = INPUT STRING                                       *
CI    HDR250(250) (C*250) = HEADERS TO SEARCH FOR IN INPUT FILE             *
C     +   HEADER MUST BE ON FORM 'THIS IS THE HEADER [N]P', WHERE N         *
C     +   IS THE NUMBER OF LINES INCLUDED IN DATA LINE (CONCATENATED).      *
C     +   IF 'N'='*' THEN ONE DATA LINE IS RETURNED PER LINE IN THE FILE    *
C     +   (NOTE: THE HEADER IS IN THIS CASE ONLY NEEDED ONCE).              *
C     +   A 'P' AT THE END IMPLIES THAT THE DATA SHOULD BE PREPROCESSED.    *
CO    INTOUT (I*4) = HEADER ID (LOC IN ARRAY) FOR DATA FOUND IN FILE *
CO    DAT250 (C*250) = DATA BODY                                     *
C     +      II=INTOUT(JJ) => HEADER HDR250(II) HAD DATABODY DAT250(JJ)     *
CIO   IRC (I*4) = ERROR RETURN CODE (IRC=0, ALL OK)                         *
C     +     A CALL WITH (IRC.NE.0) WILL INITIALISE THE ROUTINE              *
C     +                                                                     *
C     NOTE :                                                                *
C     +   o THE LINE '?' IN INPUT FILE WILL LIST ALL HEADERS AND COMMANDS   *
C     +   o IT IS POSSIBLE TO BYPASS THE MATHEMATICAL PREPROCESSOR          *
C     +     (IF IT FAILS TO WORK). IF THE MATHEMATICAL EXPRESSION IS        *
C     +     CONTAINED IN {}, IT WILL NOT BE EVALUATED ({} ARE DELETED), AN  *
C     +     ALTERNATIVE IS [] ([] ARE CONVERTED INTO ()).                   *
C     +                                                                     *
C     EXAMPLE OF CALL SEQUENCE :                                            *
C     +                                                                     *
C     INTEGER IRC,KODE                                                      *
C     CHARACTER*1000 ARG1000                                                *
C     CHARACTER*250 HDR250(250),DAT250                                      *
C     INTEGER INTOUT                                                        *
C     LOGICAL ENDOFF                                                        *
C     ARG1000='-h'                                                          *
C     HDR250(1)='THIS IS A HEADER -h[*]P'                                   *
C     KODE=-1                                                               *
C     CALL NUKEM(KODE,arg1000,HDR250,INTOUT,DAT250,ENDOFF,IRC)              *
C     KODE=0                                                                *
C     CALL NUKEM(KODE,arg1000,HDR250,INTOUT,DAT250,ENDOFF,IRC)              *
C     KODE=+1                                                               *
C     CALL NUKEM(KODE,arg1000,HDR250,INTOUT,DAT250,ENDOFF,IRC)              *
C     +                                                                     *
C     EXAMPLE OF INPUT FILE (THE 'C     ' AND '*' SHOULD BE IGNORED)        *
C     +                                                                     *
C     # first line (comment)                                                *
C     # test input file (yet another comment)                               *
C       THIS IS A HEADER :                                                  *
C        data-string data1                                                  *
C        1+2 +1.0D-4^-2 +23/1.01D2 +(1*(-1*(1*(-1*(1*(-2)))))) data2        *
C     # (...and another comment) The next line gives (some) help            *
C        ?                                                                  *
C     # last line (last comment)                                            *
C     +                                                                     *
C     THE DATA IN THE ABOVE FILE IS INTERPRETED AS :                        *
C     data-string data1                                                     *
C     3 +100000000. +0.22772277227723 +(-2) data2                           *
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
      INTEGER MAXHDR
      PARAMETER (MAXHDR=100)
C
C     INTERFACE VARIABLES
C
      INTEGER KODE
      CHARACTER*1000 ARG1000
      CHARACTER*250 HDR250(MAXHDR),DAT250
      INTEGER INTOUT,IRC
      LOGICAL ENDOFF
C
C     INTERNAL VARIABLES
C
      LOGICAL INITIALISED,ACTIVE,BDEB
      LOGICAL LFLDAT(MAXHDR) ! 
      INTEGER NROPT, MAXGRP
      PARAMETER (NROPT=5,MAXGRP=250)
      LOGICAL PPDAT(NROPT,MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR),CODHDR(MAXHDR),NRHDR,CURHDR
      INTEGER HDRGRP(6,MAXGRP),NRGRP
      LOGICAL LFLGRP(MAXGRP)
      LOGICAL IENDOFF
C
      INTEGER MAXCOM,NRCOM,
     &     LENGTH,LENB,LENF,MEAN
      PARAMETER (MAXCOM=13)
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      CHARACTER*250 NUKEHEAD
      CHARACTER*1000 BUFF1000,PATHF
      EXTERNAL FTUNIT,LENGTH
C      INTEGER CMLEN(MAXCOM),system,putenv,FTUNIT
C      EXTERNAL system,putenv
      INTEGER CMLEN(MAXCOM),FTUNIT
C
      INTEGER    LINE,CNT
C
      LOGICAL BDONE,PROCESS
C
C     AUXILIARY VARIABLES
C
      INTEGER MAXVAR,IFLVL,ARGLEN
      PARAMETER (MAXVAR=50)
      INTEGER VARLEN(MAXVAR),NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
C
C     UNIT CONTROLLERS
C
      INTEGER MAXUNIT
      PARAMETER (MAXUNIT=10)
      INTEGER UNITA(MAXUNIT),CUNIT,LUNIT(MAXUNIT),II
      CHARACTER*250 FILES(MAXUNIT)
C
      COMMON /CCNUKES/INITIALISED,ACTIVE,BDEB,
     &     LFLDAT,PPDAT,LFLGRP,ACTHDR,NRLEN,CODHDR,NRHDR,CURHDR,
     &     HDRGRP,NRGRP,IENDOFF,LENF,LENB,COM20,HLP20,BUFF1000,PATHF,
     &     CMLEN,LINE,CNT,BDONE,PROCESS,IFLVL,VARLEN,NRVAR,NAMVAR,
     &     VALVAR,UNITA,CUNIT,LUNIT,FILES,ARGLEN
C
C     EQUAL SIGN IDENTIFIERS
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'NUKES'/
C
C     Debug System.
C
      BDEB=.false.
C      BDEB=.true.
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C
      INTOUT=0
C
      IF (KODE.EQ.-1) THEN
         IF (INITIALISED) THEN
            WRITE(*,*) MYNAME,'ROUTINE IS ALLREADY INITIALISED'
            IRC=42
            RETURN
         END IF
C
C     INITIALISE
C
         LINE=-1
         ENDOFF=.FALSE.
         NRGRP=0
         IFLVL=0
         LFLGRP(:)=.FALSE.
C
C     C1=CHAR(0)
C
C     DEFINE COMMANDS
C
         CALL GETCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from GETCOM.',IRC
            RETURN
         END IF
C     
C     ANALYZE HEADERS
C
         CALL ANAHDR(MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &        CODHDR,PPDAT,MAXGRP,NRGRP,HDRGRP,
     &        IRC)
C
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from ANAHDR.',IRC
            RETURN
         END IF
C
         DO II=1,NRHDR
            LFLDAT(II)=.FALSE.
         END DO
C
C     GET FIRST LINE
C
         CNT=0
         CUNIT=1
         UNITA(CUNIT)=0
         FILES(CUNIT)='argument'
         CALL CHOP0(FILES(CUNIT),250)
         LENF=LENGTH(FILES(CUNIT),250,10)
C
         IENDOFF=.FALSE.
         call chop0(arg1000,1000)
         ARGLEN=LENGTH(ARG1000,1000,arglen)
C         write(*,*)myname,'ARGLEN2:',arglen,arg1000(240:257)
         CALL GETLINES(KODE,ARG1000,ARGLEN,MAXUNIT,LUNIT,CUNIT,
     &        CNT,MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,CODHDR,LINE,
     &        BUFF1000,LENB,IENDOFF,IRC)
C
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from GETLINES.',IRC
            RETURN
         END IF
C
         IF (IENDOFF) THEN
            IRC=266
            WRITE(*,*) MYNAME,
     &           'Use the "?" command in the input file '//
     &           'to list the possible headers.'
            WRITE(*,'(A)') '#'
            WRITE(*,'(A)') ' ?'
            WRITE(*,'(A)') '#'
            WRITE(*,*) MYNAME,'Input string is empty.',IRC
            RETURN
         END IF
C
         INITIALISED = .TRUE.
C
      ELSE IF (KODE.EQ.0) THEN  ! analyse next line
C
         IF (.NOT.INITIALISED) THEN
            WRITE(*,*) MYNAME,'ROUTINE NEEDS TO BE INITIALISED'
            IRC=44
            RETURN
         END IF
C
         INTOUT=0
         DAT250=''
C
         BDONE=IENDOFF
         DO WHILE (.NOT.BDONE)
C
            PROCESS=.TRUE.
C
C     CHECK FOR COMMAND
C
            CALL QCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,PPDAT,
     &           MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &           MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &           MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,IFLVL,
     &           MAXGRP,NRGRP,HDRGRP,LFLGRP,
     &           BUFF1000,LENB,PROCESS,BDONE,IENDOFF,IRC)
C
            IF (IRC.NE.0) THEN
               WRITE(*,*) MYNAME,'Error return from QCOM.',
     &              IRC
               RETURN
            END IF
C     
C     CHECK IF HAVE A HEADER
C
            IF (PROCESS) THEN
               CALL QHDR(MAXUNIT,FILES,LUNIT,CUNIT,
     &              MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &              LFLDAT,INTOUT,CNT,CODHDR,LINE,
     &              BUFF1000,LENB,PROCESS,BDONE,IRC)
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from QHDR.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
C     CHECK IF WE SHOULD STORE AS A DATA-BODY
C
            IF (PROCESS) THEN
               CALL QBDY(MAXUNIT,FILES,LUNIT,CUNIT,
     &              MAXHDR,HDR250,NRLEN,PPDAT,INTOUT,
     &              CNT,CODHDR,LINE,DAT250,
     &              MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,
     &              BUFF1000,LENB,PROCESS,BDONE,IRC)
C
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from QBDY.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
C     WRITE ERROR MESSAGE IF WE WERE NOT ABLE TO PROCESS LINE
C
            IF (PROCESS) THEN
               
               WRITE(*,*) MYNAME,
     &              '"'//FILES(CUNIT)(1:LENF)//'", line ',
     &              LUNIT(CUNIT),': Error: Unable to interpret:',
     &              BUFF1000(1:LENB)
               IRC=999
               RETURN
            END IF
C
C     GET NEXT LINE IF NOT EOF
C
            IF (.NOT.IENDOFF) THEN ! we may have encountered a "stop" statement
               BUFF1000=''
C               write(*,*)myname,'ARGLEN3:',arglen,arg1000(240:257)
               CALL GETLINES(KODE,ARG1000,ARGLEN,MAXUNIT,LUNIT,CUNIT,
     &              CNT,MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,CODHDR,LINE,
     &              BUFF1000,LENB,IENDOFF,IRC)
C     
               IF (IRC.NE.0) THEN
                  WRITE(*,*) MYNAME,'Error return from GETLINES.',
     &                 IRC
                  RETURN
               END IF
C
            END IF
C
            IF (IENDOFF) THEN   ! ok, wrap it up...
               BDONE=.TRUE.
               IF (LINE.NE.-1) THEN
                  IF (CNT.EQ.0 .AND. CODHDR(LINE).EQ.-1) THEN
                     LINE=-1
                     INTOUT=0
                  END IF
               ELSE
                  INTOUT=0
               END IF
            END IF
C     
         END DO
C
C     REPORT EOF IF WE ARE RETURNING DATA...
C
         ENDOFF=IENDOFF
C
         IF (INTOUT.NE.0) THEN
            LFLDAT(INTOUT)=.TRUE.
         END IF
C
      ELSE IF (KODE.EQ.1) THEN
         INITIALISED =.FALSE.
C     
C     CHECK THAT READING TERMINATED CORRECTLY
C     
C     
C     CHECK FOR EOF INSIDE FIXED FORMAT DATA-BODY...
C     
         IF (LINE.NE.-1) THEN
            IF (CODHDR(LINE).GT.0 .AND. CNT.NE.CODHDR(LINE)) THEN
               WRITE(*,*) MYNAME,
     &              'End of file interrupts data body.'
               IRC=345
               RETURN
            END IF
         END IF
C     
C     CHECK FOR NESTING ERROR IN IF-EXPRESSIONS...
C     
         IF (IFLVL.NE.0) THEN
            WRITE(*,*) MYNAME,
     &           'End of file interrupts if test.'
            IRC=345
            RETURN
         END IF
C     
C     CHECK FOR MISSING HEADERS...
C     
         CALL ANALFL(MAXHDR,NRHDR,HDR250,
     &        ACTHDR,LFLDAT,NRLEN,
     &        MAXGRP,NRGRP,HDRGRP,
     &        IRC)
         IF (IRC.EQ.391.AND.FILES(CUNIT)(1:11).EQ.'mkbccfinput') THEN
            IRC=0
            WRITE(*,*) MYNAME,' IRC reset from 391'
            WRITE(*,*) ' CHECK if problem is merely redundant headers'
         ENDIF
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from ANALFL.',
     &           IRC
            RETURN
         END IF
C     
      END IF
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine ends.',IRC
C
      RETURN
      END
#__file: 'nuketail.F' 0100664    **DO NOT DELETE**
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
#__file: 'object.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDOBJECT
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCOBJECT/ LENS
      DATA LENS /1/
      END BLOCK DATA


      SUBROUTINE OBJECT(STRING,N,START,POS,CODE)
C     ***********************************************************************
C     +                                                                     *
C     IDENTIFIES AN OBJECT (VARIABLE, OPERAND, NUMBER), AND RETURNS LOCATION*
C     +                                                                     *
CI    STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CI    START (I*4) = POSITION AT WHICH TO START SEARCH                       *
CO    POS(2) (I*4) = START AND END POSITION OF OBJECT                       *
CO    CODE (I*4) = OBJECT TYPE IDENTIFIER                                   *
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
      CHARACTER*1 XCAR
      INTEGER START,POS(2),CODE,ICODE
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'OBJECT'/
C
      INTEGER LENS,LENGTH,LEVEL,OLEVEL,JJ,CODE2,PAR,CPOS
      LOGICAL FOBJ,OFOBJ,QREAL,OQREAL,DIGIT,DEFNTNR,NEED
      EXTERNAL LENGTH,DIGIT,DEFNTNR
      COMMON /CCOBJECT/ LENS
C
      CPOS=0
      IF (CODE.EQ.1) THEN
C
C------SEARCH FORWARDS
C
         CODE2=1
         POS(1)=START
         PAR=0
C
C------IF WE PREVIOUS CHAR WAS A SIGN, SHIFT POS
C
         IF (START.GT.1) THEN
            CALL WHICH(STRING(START-1:START-1),ICODE)
            IF (ICODE.EQ.5.OR.ICODE.EQ.6) THEN
               POS(1)=START-1
            ENDIF
         ENDIF
C
C------IF WE ARE CURRENTLY AT A SIGN, MOVE FORWARDS
C
         IF (START.LT.N .AND. POS(1).EQ.START) THEN
            CALL WHICH(STRING(START:START),ICODE)
            IF(ICODE.EQ.5.OR.ICODE.EQ.6) START=START+1
         ENDIF
C
         LEVEL=0
         OLEVEL=0
         POS(2)=START
         JJ=START
         OQREAL=.TRUE.
         OFOBJ=.FALSE.
         FOBJ=.FALSE.
C
         LENS=LENGTH(STRING,N,LENS)
         QREAL=(LENS.GT.0)
         IF (JJ.LE.N.AND.JJ.GE.1) THEN
            XCAR=STRING(JJ:JJ)
            CALL WHICH(XCAR,ICODE)
         ELSE
            ICODE=1
         ENDIF
         IF (ICODE.EQ.0) THEN
            DO WHILE (JJ.LE.LENS .AND.
     &           .NOT.FOBJ .AND. LEVEL.GE.0)
C
               IF (STRING(JJ:JJ).EQ.'(') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.')') LEVEL=LEVEL-1
               IF (STRING(JJ:JJ).EQ.'[') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.']') LEVEL=LEVEL-1
               IF (STRING(JJ:JJ).EQ.'{') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.'}') LEVEL=LEVEL-1
C
               IF (LEVEL.EQ.1 .AND. JJ.EQ.START) THEN
                  PAR=LEVEL
                  POS(1)=POS(1)+1
               ENDIF
C
               IF (LEVEL.EQ.0) THEN
                  IF (JJ.LE.N.AND.JJ.GE.1) THEN
                     XCAR=STRING(JJ:JJ)
                     CALL WHICH(XCAR,ICODE)
                     FOBJ=(ICODE.NE.0)
                  ELSE
                     FOBJ=.FALSE.
                  ENDIF
                  IF (OQREAL) QREAL=DEFNTNR(STRING(JJ:JJ),CODE2,NEED)
                  IF (FOBJ.AND.OQREAL.AND.QREAL) FOBJ=.FALSE.
                  IF (.NOT.FOBJ) CPOS=JJ
                  IF (OLEVEL.NE.0) FOBJ=.TRUE.
                  OQREAL=QREAL
               ELSEIF(LEVEL.GT.0.AND.OLEVEL.EQ.0.AND.PAR.EQ.0) THEN
                  FOBJ=.TRUE.
               ELSEIF(LEVEL.GT.0) THEN
                  CODE2=+1
                  QREAL=.FALSE.
                  NEED=.FALSE.
                  FOBJ=.FALSE.
               ELSE
                  FOBJ=.TRUE.
               ENDIF
C
               IF (.NOT. FOBJ) THEN
                  IF (LEVEL.EQ.PAR) POS(2)=JJ
                  CPOS=JJ
                  OLEVEL=LEVEL
                  IF (OQREAL) OQREAL=QREAL
                  JJ=JJ+1
               ENDIF
C
            ENDDO
         ELSE
            CODE=ICODE
         ENDIF
      ELSEIF(CODE.EQ.-1) THEN
C
C-----SEARCH BACKWARDS
C
         CODE2=-1
         LEVEL=0
         OLEVEL=0
         POS(1)=START+1
         POS(2)=START
         JJ=START
         OQREAL=.TRUE.
         OFOBJ=.FALSE.
         FOBJ=.FALSE.
         PAR=0
C
         LENS=LENGTH(STRING,N,LENS)
         QREAL=(LENS.GT.0)
         IF (JJ.LE.N.AND.JJ.GE.1) THEN
            XCAR=STRING(JJ:JJ)
            CALL WHICH(XCAR,ICODE)
         ELSE
            ICODE=1
         ENDIF
         IF (ICODE.EQ.0) THEN
            DO WHILE (JJ.GE.1 .AND.
     &           .NOT.FOBJ .AND. LEVEL.LE.0)
C
               IF (STRING(JJ:JJ).EQ.'(') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.')') LEVEL=LEVEL-1
               IF (STRING(JJ:JJ).EQ.'[') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.']') LEVEL=LEVEL-1
               IF (STRING(JJ:JJ).EQ.'{') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.'}') LEVEL=LEVEL-1
C
               IF (LEVEL.EQ.-1 .AND. JJ.EQ.START) THEN
                  PAR=LEVEL
                  POS(2)=POS(2)-1
               ENDIF
C
               IF (LEVEL.EQ.0) THEN
                  IF (JJ.LE.N.AND.JJ.GE.1) THEN
                     XCAR=STRING(JJ:JJ)
                     CALL WHICH(XCAR,ICODE)
                     FOBJ=(ICODE.NE.0)
                  ELSE
                     FOBJ=.FALSE.
                  ENDIF
                  IF (OQREAL) QREAL=DEFNTNR(STRING(JJ:JJ),CODE2,NEED)
                  IF (FOBJ.AND.OQREAL.AND.QREAL)FOBJ=.FALSE.
                  IF (.NOT.FOBJ) CPOS=JJ
                  IF (OLEVEL.NE.0) FOBJ=.TRUE.
                  OQREAL=QREAL
               ELSEIF(LEVEL.LT.0.AND.OLEVEL.EQ.0.AND.PAR.EQ.0) THEN
                  FOBJ=.TRUE.
               ELSEIF(LEVEL.LT.0) THEN
                  CODE2=-1
                  QREAL=.FALSE.
                  NEED=.FALSE.
                  FOBJ=.FALSE.
               ELSE
                  FOBJ=.TRUE.
               ENDIF
C
               IF (.NOT. FOBJ) THEN
                  IF (LEVEL.EQ.PAR) POS(1)=JJ
                  CPOS=JJ
                  OLEVEL=LEVEL
                  IF (OQREAL) OQREAL=QREAL
                  JJ=JJ-1
               ENDIF
C
            ENDDO
C
C------IF WE PREVIOUS CHAR WAS A SIGN, SHIFT POS
C
            IF (CPOS.GT.1.AND.
     &           STRING(CPOS:CPOS).NE.'-'.AND.
     &           STRING(CPOS:CPOS).NE.'+') THEN
               CALL WHICH(STRING(CPOS-1:CPOS-1),ICODE)
               IF (ICODE.EQ.5.OR.ICODE.EQ.6) THEN
                  CPOS=CPOS-1
               ENDIF
            ENDIF
         ELSE
            CODE=ICODE
         ENDIF
      ELSE
         WRITE(*,*) MYNAME,'WRONG CODE',CODE
      ENDIF
C
      START=CPOS
      IF (CPOS.GE.1 .AND. CPOS.LE.N) THEN
         CODE=0
      ELSE
         CODE=ICODE
      ENDIF
C
      IF (CODE.EQ.0) THEN
C     CHECK IF OBJECT IS SURROUNDED BY PARANTHESIS
         IF (STRING(POS(1):POS(1)).EQ.'(') POS(1)=POS(1)+1
         IF (STRING(POS(2):POS(2)).EQ.')') POS(2)=POS(2)-1
      ENDIF
C
      RETURN
      END
#__file: 'perff.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDPERFF
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCPERFF/ LENS
      DATA LENS /1/
      END BLOCK DATA


      SUBROUTINE PERFF(STRING,N,CHANGED,IRC)
C     ***********************************************************************
C     +                                                                     *
C     PERFORMS MATHEMATICAL OPERATIONS ON STRING                            *
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
C
      COMMON /CCPERFF/ LENS
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
               CALL EXECF(STRING,N,CHANGED,POS)
               if (changed) call cleansome(string,n,pos)
            ELSE
C     EXECUTE CALC, AND REPLACE PARANTHESIS WITH []
               STRING(POS(1):POS(1))='['
               STRING(POS(2):POS(2))=']'
               CALL EXECF(STRING,N,CHANGED,POS)
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
#__file: 'perfl.F' 0100664    **DO NOT DELETE**
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
#__file: 'perfm.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDPERFM
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCPERFM/ LENS
      DATA LENS /1/
      END BLOCK DATA


      SUBROUTINE PERFM(STRING,N,CHANGED,IRC)
C     ***********************************************************************
C     +                                                                     *
C     PERFORMS MATHEMATICAL OPERATIONS ON STRING                            *
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
C
      COMMON /CCPERFM/ LENS
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
               CALL EXECM(STRING,N,CHANGED,POS)
               if (changed) call cleansome(string,n,pos)
            ELSE
C     EXECUTE CALC, AND REPLACE PARANTHESIS WITH []
               STRING(POS(1):POS(1))='['
               STRING(POS(2):POS(2))=']'
               CALL EXECM(STRING,N,CHANGED,POS)
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
#__file: 'qbdy.F' 0100664    **DO NOT DELETE**
      subroutine QBDY(MAXUNIT,FILES,LUNIT,CUNIT,
     &     MAXHDR,HDR250,NRLEN,PPDAT,INTOUT,
     &     CNT,CODHDR,LINE,DAT250,
     &     MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,
     &     BUFF1000,LENB,PROCESS,BDONE,IRC)
C     
      implicit none
C     
      INTEGER MAXUNIT
      CHARACTER*250 FILES(MAXUNIT)
      INTEGER UNITA(MAXUNIT),LUNIT(MAXUNIT),CUNIT
      INTEGER MAXHDR,NROPT
      CHARACTER*250 HDR250(MAXHDR)
      PARAMETER (NROPT=5)
      LOGICAL PPDAT(NROPT,MAXHDR)
      INTEGER INTOUT, CNT,CODHDR(MAXHDR),LINE
      CHARACTER*250 DAT250
      INTEGER NRLEN(3,MAXHDR),MAXVAR,NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
      INTEGER VARLEN(MAXVAR)
      CHARACTER*1000 BUFF1000
      INTEGER LENB
      LOGICAL PROCESS, BDONE
      INTEGER IRC
C     
      CHARACTER*16 MYNAME
      DATA MYNAME /'QBDY'/
      INTEGER LENGTH,LENF,LEND
      EXTERNAL LENGTH
      CHARACTER*1000 DUMP
      LOGICAL ERROR
C     
C     PRE-PROCESS LINE IF THIS IS REQUIRED
C     
      ERROR=.FALSE.
      IF (LINE.NE.-1) THEN
         PROCESS=.FALSE.
         CALL EVAL(BUFF1000,1000,NRVAR,NAMVAR,VALVAR,VARLEN,
     &        PPDAT(1,LINE),IRC)
         IF (IRC.NE.0) THEN
            IRC=0
            ERROR=.TRUE.
            LENB=LENGTH(BUFF1000,250,10)
            BUFF1000='Error return from EVAL '//
     &           BUFF1000(1:LENB)//').'
         ELSE
            LENB=LENGTH(BUFF1000,1000,LENB)
            IF (LENB.EQ.0) THEN
C     DATA LINE IS EMPTY
               WRITE(*,*) MYNAME,
     &              'Warning: empty data line found under: '//
     &              HDR250(LINE)(1:NRLEN(1,LINE))
            ELSE
C     
C     DO NOT ALLOW RESIDUAL VARIABLES
C     
               CALL QVAR(BUFF1000,1000,
     &              NRVAR,NAMVAR,VALVAR,VARLEN,IRC)
               IF (IRC.NE.0) THEN
                  ERROR=.TRUE.
                  IF (.NOT. PPDAT(1,LINE)) THEN ! variable substitution not active in header
                     BUFF1000='Header does not use '
     &                    //'variable substitution?'
                  ELSE
                     BUFF1000='Undefined variable.'
                  END IF
               ELSE
C     
C     STORE DATA LINE
C     
                  BDONE=.TRUE.
C     
                  INTOUT=LINE
                  DAT250=BUFF1000(1:250)
C     
                  CNT=CNT+1
C     
C     CHECK IF WE ARE ACCUMULATING TOO MANY LINES
C     
                  IF (CODHDR(LINE).NE.-1 .AND. 
     &                 CNT.GT.CODHDR(LINE)) THEN
                     ERROR=.TRUE.
                     write(BUFF1000,'(A,I5,A,I5,4(A))')
     &                    'Too many data lines, found:',
     &                    cnt,', expected:',CODHDR(LINE),
     &                    ', Header:',HDR250(LINE)
     &                    (1:NRLEN(1,LINE)),' Body:',
     &                    dat250
                  END IF
               END IF
C     
            ENDIF
         ENDIF
      ENDIF
C     
      IF (ERROR) THEN
         CALL CHOP0(FILES(CUNIT),250)
         LENF=LENGTH(FILES(CUNIT),250,10)
         CALL CHOP0(BUFF1000,250)
         LENB=LENGTH(BUFF1000,250,10)
         IF (LENF.GT.80)LENF=80
         WRITE(DUMP,*) 
     &        '"'//FILES(CUNIT)(1:LENF)//'", line ',
     &        LUNIT(CUNIT),': Error: '//BUFF1000(1:LENB)
         CALL CHOP0(DUMP,200)
         LEND=LENGTH(DUMP,200,20)
         WRITE(*,*) DUMP(1:LEND)
         IRC=291
         RETURN
      ENDIF
C     
      RETURN
      END
#__file: 'qcom.F' 0100664    **DO NOT DELETE**
c Library:strgen4 $Id: file.f,v 1.1 2012-12-10 12:03:54 franktt Exp $
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


#__file: 'qhdr.F' 0100664    **DO NOT DELETE**
      SUBROUTINE QHDR(MAXUNIT,FILES,LUNIT,CUNIT,
     &     MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,LFLDAT,
     &     INTOUT,CNT,CODHDR,LINE,
     &     BUFF1000,LENB,
     &     PROCESS,BDONE,IRC)
C
      IMPLICIT NONE
C
      INTEGER MAXUNIT,CUNIT,LUNIT(MAXUNIT)
      CHARACTER*250 FILES(MAXUNIT)
      INTEGER MAXHDR,NRHDR
      CHARACTER*250 HDR250(MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR)
      LOGICAL LFLDAT(MAXHDR)
      INTEGER INTOUT,CNT,CODHDR(MAXHDR),LINE
      CHARACTER*1000 BUFF1000
      INTEGER LENB
      LOGICAL PROCESS,BDONE
      INTEGER IRC
C
      INTEGER JJ,KK,LL,LENF,LEND,LENGTH
      EXTERNAL LENGTH
      LOGICAL FOUND,ERROR
      CHARACTER*1000 DUMP
      CHARACTER*16 MYNAME
      DATA MYNAME /'QHDR'/
C
      JJ=1
      ERROR=.FALSE.
      FOUND=.FALSE.
      DO WHILE(.NOT.FOUND .AND. JJ.LE.MAXHDR)
         FOUND=((NRLEN(1,JJ).LE.LENB).AND.(ACTHDR(JJ)))
         KK=1
         DO WHILE(KK.LE.NRLEN(1,JJ).AND.FOUND)
            IF(FOUND) 
     &           FOUND=(BUFF1000(KK:KK).EQ.HDR250(JJ)(KK:KK))
            KK=KK+1
         ENDDO
         if (ACTHDR(JJ).and.nrlen(2,jj).le.nrlen(3,jj).and.
     &        .not.found) then ! check if we have short header
            FOUND=((NRLEN(3,JJ)-nrlen(2,jj)+1.eq.LENB)
     &           .AND.(ACTHDR(JJ)))
            KK=nrlen(2,jj)
            LL=KK-NRLEN(2,JJ)+1
            DO WHILE(KK.LE.NRLEN(3,JJ).AND.LL.LE.1000.AND.FOUND)
               IF(FOUND) 
     &              FOUND=(BUFF1000(LL:LL).EQ.HDR250(JJ)(KK:KK))
               KK=KK+1
               LL=KK-NRLEN(2,JJ)+1
            ENDDO
         end if
         IF (FOUND)THEN
            PROCESS=.FALSE.
C     
C     CHECK IF WE ARE INTERUPTING ANOTHER DATA BODY
C     
            IF (LINE.NE.-1) THEN
               IF (CODHDR(LINE).NE.-1 .AND. 
     &              CNT.NE.CODHDR(LINE)) THEN
                  ERROR=.TRUE.
                  BUFF1000='header in data body'
               END IF
            ENDIF
C     
C     PREPARE FOR DATABODY
C     
            LINE=JJ
            CNT=0
            INTOUT=LINE
            IF (CODHDR(LINE).EQ.0) THEN
               BDONE=.TRUE.
            ENDIF
         ELSE
            JJ=JJ+1
         ENDIF
      ENDDO
C
      IF (ERROR) THEN
         CALL CHOP0(FILES(CUNIT),250)
         LENF=LENGTH(FILES(CUNIT),250,10)
         CALL CHOP0(BUFF1000,250)
         LENB=LENGTH(BUFF1000,250,10)
         IF (LENF.GT.80)LENF=80
         WRITE(DUMP,*) 
     &        '"'//FILES(CUNIT)(1:LENF)//'", line ',
     &        LUNIT(CUNIT),': Error: '//BUFF1000(1:LENB)
         CALL CHOP0(DUMP,200)
         LEND=LENGTH(DUMP,200,20)
         WRITE(*,*) DUMP(1:LEND)
         IRC=291
         RETURN
      ENDIF
C
      RETURN
      END

#__file: 'qlfl.F' 0100664    **DO NOT DELETE**
      logical function QLFL(MAXGRP,NRGRP,
     &     BHDRGRPA,BHDRGRPB)
C
      implicit none
C
      integer maxgrp, nrgrp
      integer BHDRGRPA(2,maxgrp),BHDRGRPB(2,maxgrp)
C
      logical later,bdone
      integer ii
C
      II=1
      later=.TRUE.
      bdone=(ii.gt.nrgrp)
      DO WHILE (.not. bdone)
         if (bhdrgrpa(1,ii).EQ.bhdrgrpb(1,ii) .and.
     &        bhdrgrpa(2,ii).EQ.bhdrgrpb(2,ii)) then
            ii=ii+1
         else if (bhdrgrpa(2,ii).lt.bhdrgrpb(2,ii) .OR.
     &           (bhdrgrpa(1,ii).lt.bhdrgrpb(1,ii) .and.
     &        bhdrgrpa(2,ii).EQ.bhdrgrpb(2,ii))) then
            later=.true.
            bdone=.true.
         else
            later=.false.
            bdone=.true.
         end if
         if (.not.bdone) bdone=(ii.gt.nrgrp)
      END DO
C
      qlfl=later
      return
      end
#__file: 'qvar.F' 0100664    **DO NOT DELETE**
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
#__file: 'reperr.F' 0100664    **DO NOT DELETE**
      SUBROUTINE REPERR (EGRP,CERR50,MXHDR,HDR250,
     &        MXGRP,NRGRP,HDRGRP,LFLGRP,
     &        IRC)
C
      IMPLICIT NONE
C
      INTEGER EGRP            ! group with error
      CHARACTER*50 CERR50       ! error message
      INTEGER MXHDR           ! max number of headers
      CHARACTER*250 HDR250(MXHDR) ! list of headers
      INTEGER MXGRP           ! MAX GROUPS
      INTEGER NRGRP           ! NUMBER OF GROUPS
      INTEGER HDRGRP(6,MXGRP) ! 1=parent, 2=next sibling, 3=first child,  4=type, 5=id/chdr, 6=lvl
      LOGICAL LFLGRP(MXGRP) ! is group set?
      INTEGER IRC
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'REPERR'/
C
      INTEGER CIND(MXGRP),POS,LVL,TIND
      LOGICAL BDONE,BBDONE
      CHARACTER*250 PRE,PREF
      INTEGER LENH,LENP,LENGTH
      EXTERNAL LENGTH
C
      DATA PRE /'                                            '/
C     
      WRITE(*,*) MYNAME,'======================================'
      LVL=1                     ! LEVEL (MUST HERE BE 1)
      CIND(LVL)=1               ! START WITH FIRST TOP LEVEL GROUP
      BDONE=.FALSE.
      DO WHILE (.NOT. BDONE)    ! LOOP OVER SUB GRP'S
         IF (CIND(LVL).EQ.0) THEN ! end of a chain reached
            IF (LVL.GT.1) THEN  ! move up
               CIND(LVL)=0
               LVL=LVL-1
               CIND(LVL)=HDRGRP(2,CIND(LVL)) ! move to next sibling
            ELSE
               BDONE=.TRUE.
            END IF
         ELSE
            POS=CIND(LVL)
            IF (POS.EQ.EGRP) THEN
               LENP=LENGTH(CERR50,50,10)
               PREF=CERR50(1:LENP)//' ============'//
     &              '====================================='
               CALL CHOP0(PREF,250)
               LENP=25+2*LVL
               PREF(LENP:LENP)='>'
            ELSE
               LENP=25+2*LVL
               PREF=PRE(1:LENP)
            END IF
            IF (LFLGRP(POS)) THEN
               PREF(LENP-1:LENP-1) = '*'
            END IF
            IF (HDRGRP(4,POS).EQ.1) THEN
               write (*,*) PREF(1:LENP)//
     &              '[all of the following:]'
            ELSE IF (HDRGRP(4,POS).EQ.2) THEN
               write (*,*) PREF(1:LENP)//
     &              '[up to one of the following:]'
            ELSE IF (HDRGRP(4,POS).EQ.3) THEN
               write (*,*) PREF(1:LENP)//'[possibly:]'
            ELSE IF (HDRGRP(4,POS).EQ.-1) THEN
               lenh=length(hdr250(-hdrgrp(5,POS)),250,10)
               write (*,*) PREF(1:LENP)//
     &              hdr250(-hdrgrp(5,POS))(1:LENH)
            END IF
            IF (HDRGRP(3,POS).NE.0) THEN
               LVL=LVL+1
               CIND(LVL)=HDRGRP(3,POS) ! first child
            ELSE
               CIND(LVL)=HDRGRP(2,CIND(LVL)) ! next sibling
            END IF
         END IF
      END DO
      WRITE(*,*) MYNAME,'======================================'
C     
      return
      end
#__file: 'reset.F' 0100664    **DO NOT DELETE**
      SUBROUTINE RESET(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     ERASES CONTENTS OF STRING                                             *
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
      CHARACTER*1 C1
      INTEGER II
C
      C1=CHAR(0)
      DO II=1,N
         STRING(II:II)=C1
      ENDDO
      RETURN
      END
#__file: 'slavel.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDSLAVEL
      IMPLICIT NONE
      INTEGER LEN1,LEN2
      COMMON /CCSLAVEL/LEN1,LEN2
      DATA LEN1 /1/,LEN2 /1/
      END BLOCK DATA


      SUBROUTINE SLAVEL(STRING1,STRING2,STRING3,OP,CLEAN,CHANGED,IRC)
C     ***********************************************************************
C     +                                                                     *
C     EXECUTES MATHEMATICAL OPERATION 'OP' ON STRING1 AND STRING2.          *
C     THE RESULT IS RETURNED IN STRING3                                     *
C     +                                                                     *
CI    STRING1 (C*250) = FIRST ARGUMENT                                      *
CI    STRING2 (C*250) = SECOND ARGUMENT                                     *
CO    STRING3 (C*250) = RESULT                                              *
CI    OP (I*4) = OPERATION,                                                 *
C     +      =1  ==                                                         *
C     +      =2  >=                                                         *
C     +      =3  <=                                                         *
C     +      =4  <>                                                         *
C     +      =5  >                                                          *
C     +      =6  <                                                          *
C     +      =7  !                                                          *
C     +      =8  &&                                                         *
C     +      =9  ||                                                         *
CIO   CLEAN (L*4) = .TRUE. IF NO BRACKETS SHOULD BE ADDED (IF NOT EVAL)     *
CO    IRC=ERROR RETURN CODE (IRC=0 OK, IRC=1 IF NO OPERATION)               *
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
      CHARACTER*250 STRING1,STRING2,STRING3
      CHARACTER*250 STRB1,STRB2
      LOGICAL CLEAN,CHANGED
      INTEGER OP,IRC
C
      INTEGER LEN1,LEN2,LEN3,LENGTH,ISI(2)
      REAL ISR(2)
C
      LOGICAL ISINT,ISREAL,QINT(2),QREA(2),QLOG(2),
     &     CAMEWS,PSGN(2),ATOM,ISL(2),RRL,DONE,ISLOG
      EXTERNAL LENGTH,ISINT,ISREAL,ISLOG,ATOM
C
      common /ccslavel/LEN1,LEN2
C
C     INITIALIZE
C
      IRC=0
      CALL CHOP0(STRING1,250)
      CALL CHOP0(STRING2,250)
      CALL RESET(STRING3,250)
C
      STRB1=STRING1
      STRB2=STRING2
C
      LEN1=LENGTH(STRB1,250,LEN1)
      LEN2=LENGTH(STRB2,250,LEN2)
C
      CAMEWS=(STRB1(1:1).EQ.'-'.OR.STRB1(1:1).EQ.'+')
C
      IF (STRB1(1:1).EQ.'-') THEN
         PSGN(1)=.FALSE.
         STRB1=STRB1(2:LEN1)
         LEN1=LEN1-1
      ELSEIF (STRB1(1:1).EQ.'+') THEN
         PSGN(1)=.TRUE.
         STRB1=STRB1(2:LEN1)
         LEN1=LEN1-1
      ELSE
         PSGN(1)=.TRUE.
      ENDIF
C
      IF (STRB2(1:1).EQ.'-') THEN
         PSGN(2)=.FALSE.
         STRB2=STRB2(2:LEN2)
         LEN2=LEN2-1
      ELSEIF (STRB2(1:1).EQ.'+') THEN
         PSGN(2)=.TRUE.
         STRB2=STRB2(2:LEN2)
         LEN2=LEN2-1
      ELSE
         PSGN(2)=.TRUE.
      ENDIF
C
      IF (STRB1(1:1).EQ.'['.AND.STRB1(LEN1:LEN1).EQ.']') THEN
         IF (ATOM(STRB1(2:LEN1-1),LEN1-2)) THEN
            STRB1=STRB1(2:LEN1-1)
         ENDIF
      ENDIF
C
      IF (STRB2(1:1).EQ.'['.AND.STRB2(LEN2:LEN2).EQ.']') THEN
         IF (ATOM(STRB2(2:LEN2-1),LEN2-2)) THEN
            STRB2=STRB2(2:LEN2-1)
         ENDIF
      ENDIF
C
C     FIND OUT IF NUMBERS ARE INTEGERS OR REAL
C
      QINT(1)=ISINT(STRB1,ISI(1))
      QINT(2)=ISINT(STRB2,ISI(2))
      QREA(1)=ISREAL(STRB1,ISR(1))
      QREA(2)=ISREAL(STRB2,ISR(2))
      QLOG(1)=ISLOG(STRB1,ISL(1))
      QLOG(2)=ISLOG(STRB2,ISL(2))
C
      DONE=.FALSE.
C
      IF (QINT(1).AND.QINT(2)) THEN
         IF (OP.EQ.1) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).EQ.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.2) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).GE.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.3) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).LE.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.4) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).NE.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.5) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).GT.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.6) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).LT.ISI(2)
            DONE=.TRUE.
         ENDIF
      ELSEIF ((QINT(1).AND.QREA(2)).OR.
     &        (QINT(2).AND.QREA(1)).OR.
     &        (QREA(1).AND.QREA(2))) THEN
         IF (QINT(1)) ISR(1)=REAL(ISI(1))
         IF (QINT(2)) ISR(2)=REAL(ISI(2))
         IF (OP.EQ.1) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).EQ.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.2) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).GE.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.3) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).LE.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.4) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).NE.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.5) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).GT.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.6) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).LT.ISR(2)
            DONE=.TRUE.
         ENDIF
      ELSE
         IF (OP.EQ.1.AND.STRING1.EQ.STRING2) THEN
            RRL=.TRUE.
            DONE=.TRUE.
         ELSEIF (OP.EQ.2 .AND. STRING1.EQ.STRING2) THEN
            RRL=.TRUE.
            DONE=.TRUE.
         ELSEIF (OP.EQ.3 .AND. STRING1.EQ.STRING2) THEN
            RRL=.TRUE.
            DONE=.TRUE.
         ELSE
            IF  (QLOG(2) .AND.
     &           (OP.EQ.7.OR.(QLOG(1).AND.OP.GE.8))) THEN
               IF (OP.EQ.7) THEN
                  RRL=(.NOT.ISL(2))
                  DONE=.TRUE.
               ELSEIF (OP.EQ.8) THEN
                  RRL=(ISL(1).AND.ISL(2))
                  DONE=.TRUE.
               ELSEIF (OP.EQ.9) THEN
                  RRL=(ISL(1).OR.ISL(2))
                  DONE=.TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      IF (DONE) THEN
         CHANGED=.TRUE.
         IF (RRL) THEN
            STRING3='T'
         ELSE
            STRING3='F'
         ENDIF
         IF (CAMEWS) THEN
            IF (PSGN(1)) THEN
               STRING3='+'//STRING3(1:249)
            ELSE
               STRING3='-'//STRING3(1:249)
            ENDIF
         ENDIF
         IF (OP.EQ.7) THEN
            LEN1=LENGTH(STRING1,250,LEN1)
            STRING3=STRING1(1:LEN1)//STRING3(1:250-LEN1)
            CALL CHOP0(STRING3,250)
         ENDIF
      ELSE
C
C     ADD PROTECTIVE BRACKETS...
C
         LEN1=LENGTH(STRING1,250,LEN1)
         LEN2=LENGTH(STRING2,250,LEN1)
         IF (OP.EQ.1) THEN
            STRING3=STRING1(1:LEN1)//'=='//STRING2(1:LEN2)
         ELSEIF (OP.EQ.2) THEN
            STRING3=STRING1(1:LEN1)//'>='//STRING2(1:LEN2)
         ELSEIF (OP.EQ.3) THEN
            STRING3=STRING1(1:LEN1)//'<='//STRING2(1:LEN2)
         ELSEIF (OP.EQ.4) THEN
            STRING3=STRING1(1:LEN1)//'<>'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.5) THEN
            STRING3=STRING1(1:LEN1)//'>'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.6) THEN
            STRING3=STRING1(1:LEN1)//'<'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.7) THEN
            STRING3=STRING1(1:LEN1)//'!'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.8) THEN
            STRING3=STRING1(1:LEN1)//'&&'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.9) THEN
            STRING3=STRING1(1:LEN1)//'||'//STRING2(1:LEN2)
         ENDIF
C
C     NO OPERATION WAS PERFORMED
C
         IRC=105
C
         IF (.NOT.CLEAN) THEN
            CALL CHOP0(STRING3,250)
            LEN3=LENGTH(STRING3,250,LEN1+LEN2+1)
            STRING3='{'//STRING3(1:LEN3)//'}'
         ENDIF
      ENDIF
      RETURN
      END
#__file: 'slavem.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDSLAVEM
      IMPLICIT NONE
      INTEGER LEN1,LEN2
      LOGICAL ACTIVE,BDEB
      COMMON /CCSLAVEM/LEN1,LEN2,ACTIVE,BDEB
      DATA LEN1 /1/,LEN2 /1/,ACTIVE /.FALSE./
      END BLOCK DATA


      SUBROUTINE SLAVEM(STRING1,STRING2,STRING3,OP,CLEAN,CHANGED,IRC)
C     ***********************************************************************
C     +                                                                     *
C     EXECUTES MATHEMATICAL OPERATION 'OP' ON STRING1 AND STRING2.          *
C     THE RESULT IS RETURNED IN STRING3                                     *
C     +                                                                     *
CI    STRING1 (C*250) = FIRST ARGUMENT                                      *
CI    STRING2 (C*250) = SECOND ARGUMENT                                     *
CO    STRING3 (C*250) = RESULT                                              *
CI    OP (I*4) = OPERATION,                                                 *
C     +      =1  ^                                                          *
C     +      =2  *                                                          *
C     +      =3  /                                                          *
C     +      =4  +                                                          *
CIO   CLEAN (L*4) = .TRUE. IF NO BRACKETS SHOULD BE ADDED (IF NOT EVAL)     *
CO    IRC=ERROR RETURN CODE (IRC=0 OK, IRC=1 IF NO OPERATION)               *
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
      CHARACTER*250 STRING1,STRING2,STRING3
      CHARACTER*250 STRB1,STRB2
      LOGICAL CLEAN,CHANGED
      INTEGER OP,IRC
C
      INTEGER LEN1,LEN2,LEN3,LENGTH,ISI(2),RRI
      REAL ISR(2),RRR
C
      LOGICAL ISINT,ISREAL,QINT(2),QREA(2),
     &     CAMEWS,PSGN(2),PST,ATOM,DONE, GOINT
      EXTERNAL LENGTH,ISINT,ISREAL,ATOM
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'SLAVEM'/
C
      LOGICAL ACTIVE,BDEB
C
      common /ccslavem/LEN1,LEN2,ACTIVE,BDEB
C
C
      BDEB=.false.
C      BDEB=.true.
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C
C     INITIALIZE
C
      IRC=0
      CALL CHOP0(STRING1,250)
      CALL CHOP0(STRING2,250)
      CALL RESET(STRING3,250)
C
      STRB1=STRING1
      STRB2=STRING2
C
      LEN1=LENGTH(STRB1,250,LEN1)
      LEN2=LENGTH(STRB2,250,LEN2)
C
C
      CAMEWS=(STRB1(1:1).EQ.'-'.OR.STRB1(1:1).EQ.'+')
C
      IF (STRB1(1:1).EQ.'-') THEN
         PSGN(1)=.FALSE.
         STRB1=STRB1(2:LEN1)
         LEN1=LEN1-1
      ELSEIF (STRB1(1:1).EQ.'+') THEN
         PSGN(1)=.TRUE.
         STRB1=STRB1(2:LEN1)
         LEN1=LEN1-1
      ELSE
         PSGN(1)=.TRUE.
      ENDIF
C
      IF (STRB2(1:1).EQ.'-') THEN
         PSGN(2)=.FALSE.
         STRB2=STRB2(2:LEN2)
         LEN2=LEN2-1
      ELSEIF (STRB2(1:1).EQ.'+') THEN
         PSGN(2)=.TRUE.
         STRB2=STRB2(2:LEN2)
         LEN2=LEN2-1
      ELSE
         PSGN(2)=.TRUE.
      ENDIF
C
      IF (STRB1(1:1).EQ.'['.AND.STRB1(LEN1:LEN1).EQ.']') THEN
         IF (ATOM(STRB1(2:LEN1-1),LEN1-2)) THEN
            STRB1=STRB1(2:LEN1-1)
         ENDIF
      ENDIF
C
      IF (STRB2(1:1).EQ.'['.AND.STRB2(LEN2:LEN2).EQ.']') THEN
         IF (ATOM(STRB2(2:LEN2-1),LEN2-2)) THEN
            STRB2=STRB2(2:LEN2-1)
         ENDIF
      ENDIF
C
C     FIND OUT IF NUMBERS ARE INTEGERS OR REAL
C
      QINT(1)=ISINT(STRB1,ISI(1))
      QINT(2)=ISINT(STRB2,ISI(2))
      QREA(1)=ISREAL(STRB1,ISR(1))
      QREA(2)=ISREAL(STRB2,ISR(2))
C
      DONE=.FALSE.
C
C     MAKE SURE WE DON'T RETURN AN INTEGER (0) FOR '2^(-2)'
      GOINT=(QINT(1).AND.QINT(2)) ! both numbers must be integers
      IF (GOINT.AND.OP.EQ.1) GOINT=(ISI(2).GE.0) ! negative exponents are always treated as real
      IF (GOINT.AND.OP.EQ.3) GOINT=(ISI(2).NE.0) ! we can not divide by 0
      IF (GOINT.AND.OP.EQ.3) GOINT=(MOD(ISI(1),ISI(2)).NE.0) ! check that division product is integer
      IF (GOINT) THEN
C     BOTH NUMBERS ARE INTEGERS
         IF (OP.EQ.1) THEN      ! ^
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRI=ISI(1)**ISI(2)
            IF (.NOT.PSGN(1)) RRI=-RRI
            DONE=.TRUE.
         ELSEIF (OP.EQ.2) THEN  ! *
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRI=ISI(1)*ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.3) THEN ! /
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRI=ISI(1)/ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.4) THEN ! +
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRI=ISI(1)+ISI(2)
            DONE=.TRUE.
         ENDIF
         WRITE(STRING3,*)RRI
         PST=(.NOT.(RRI.LT.0))
      ELSEIF ((QREA(1).OR.QINT(1)).AND.
     &        (QREA(2).OR.QINT(2))) THEN
C     CONVERT TO REAL
         IF (QINT(1)) ISR(1)=REAL(ISI(1))
         IF (QINT(2)) ISR(2)=REAL(ISI(2))
         IF (OP.EQ.1) THEN
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRR=ISR(1)**ISR(2)
            IF (.NOT.PSGN(1)) RRR=-RRR
            DONE=.TRUE.
         ELSEIF (OP.EQ.2) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRR=ISR(1)*ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.3) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRR=ISR(1)/ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.4) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRR=ISR(1)+ISR(2)
            DONE=.TRUE.
         ENDIF
         WRITE(STRING3,*)RRR
         PST=(.NOT.(RRR.LT.0))
      ENDIF
C
      IF (DONE) THEN
C
         CHANGED=.TRUE.
         CALL CHOP0(STRING3,250)
         IF (PST.AND.CAMEWS) THEN
            STRING3='+'//STRING3(1:249)
         ENDIF
C
      ELSE
C
C     ADD PROTECTIVE BRACKETS (OR ELSE '1+(2+3)^b'='6^b'
C
         IF (OP.EQ.1) THEN
            STRING3=STRB1(1:LEN1)//'^'//STRB2(1:LEN2)
         ELSEIF (OP.EQ.2) THEN
            STRING3=STRB1(1:LEN1)//'*'//STRB2(1:LEN2)
         ELSEIF (OP.EQ.3) THEN
            STRING3=STRB1(1:LEN1)//'/'//STRB2(1:LEN2)
         ELSEIF (OP.EQ.4) THEN
            LEN2=LENGTH(STRING2,250,LEN2)
            STRING3=STRB1(1:LEN1)//STRING2(1:LEN2)
         ENDIF
C
C     NO OPERATION WAS PERFORMED
C
         IRC=106
C
         IF (.NOT.CLEAN) THEN
            CALL CHOP0(STRING3,250)
            LEN3=LENGTH(STRING3,250,LEN1+LEN2+1)
            STRING3='{'//STRING3(1:LEN3)//'}'
         ENDIF
C
         IF (CAMEWS) THEN
            IF (PSGN(1)) THEN
               STRING3='+'//STRING3(1:249)
            ELSE
               STRING3='-'//STRING3(1:249)
            ENDIF
         ENDIF
      ENDIF
C
      IF (BDEB) THEN
         LEN2=LENGTH(STRING2,250,LEN2)
         LEN3=LENGTH(STRING3,250,LEN3)
         WRITE(*,*) MYNAME,'Debug: A1:',
     &     STRING1(1:LEN1),' A2:',STRING2(1:LEN2),
     &     ' A3:',STRING3(1:LEN3),' OP:',OP,goint
      ENDIF
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C
      RETURN
      END
#__file: 'substr.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDSUBSTR
      IMPLICIT NONE
      INTEGER LENS1,LENS2
      COMMON /CCSUBSTR/LENS1,LENS2
      DATA LENS1/1/,LENS2/1/
      END BLOCK DATA


      SUBROUTINE SUBSTR(STRING1,N1,STRING2,N2,POS1,DELL)
C     ***********************************************************************
C     +                                                                     *
C     SUBSTITUTES PART OF STRING1 WITH STRING2                              *
C     +                                                                     *
CIO   STRING1 (C*N1) = STRING WHICH WILL HAVE A SUBSTRING REPLACED          *
CI    N1 (I*4) = LENGTH OF STRING1                                          *
CI    STRING2 (C*N2) = STRING WHICH WILL BE COPIED OVER TO STRING1          *
CI    N2 (I*4) = LENGTH OF STRING2                                          *
CI    POS1 (I*4) = START POSITION IN STRING1                                *
CI    DELL (I*4) = NUMBER OF CHARACTERS TO DELETE IN STRING1                *
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
      INTEGER N1,N2,POS1,DELL
      CHARACTER*(*) STRING1,STRING2
C
      INTEGER LENS1,LENS2,LENGTH,MAXX,TRG,DELTA,POSS,LL
      EXTERNAL LENGTH

C     common /ccSUBSTR/LENS1,LENS2
C
C     REMOVE VALUABLE DATA FROM TARGET AREA
C
C     CALL CHOP0(STRING1,N1)
      LENS1=LENGTH(STRING1,N1,LENS1)
C
      CALL CHOP0(STRING2,N2)
      LENS2=LENGTH(STRING2,N2,LENS2)
C
      IF((POS1+LENS2-1).GT.N1)LENS2=N1+1-POS1
      IF (POS1+DELL-1.GT.LENS1)DELL=LENS1-POS1+1
C
      DELTA=(LENS2-DELL)
C
      MAXX=N1
      MAXX=MIN(MAXX,LENS1+DELTA)
      MAXX=MAXX-DELTA
C
C     MAXX IS LAST DATA (STILL IN ARRAY)
C
      TRG=POS1+DELL
      IF (DELTA.GT.0) THEN
         DO LL=MAXX,TRG,-1
            STRING1(LL+DELTA:LL+DELTA)=STRING1(LL:LL)
         ENDDO
      ELSE
         DO LL=TRG,MAXX
            STRING1(LL+DELTA:LL+DELTA)=STRING1(LL:LL)
         ENDDO
C
C     REMOVE OLD DATA IN EMPTY SLOT (AFTER LAST DATA)
C
         MAXX=MAXX+DELTA+1
         DO LL=MAXX,LENS1
            STRING1(LL:LL)=' '
         ENDDO
      ENDIF
C
      TRG=POS1+LENS2-1
      IF (TRG.GT.N1) TRG=N1
      DO LL=POS1,TRG
         POSS=LL-POS1+1
         STRING1(LL:LL)=STRING2(POSS:POSS)
      ENDDO
C
      RETURN
      END
#__file: 'system.F' 0100664    **DO NOT DELETE**
      integer function system(char250)
      implicit none
      character*250 char250
      write(*,*) 'SYSTEM not available.'
      system=999
      return
      end function system
#__file: 'truncx0.F' 0100664    **DO NOT DELETE**
      SUBROUTINE TRUNCX0(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES TRAILING 0 FROM EXPRESSION                                    *
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
      CHARACTER*1 BUFF
C
      INTEGER II,LENGTH,LENS,DELL,IIZERO
      LOGICAL DIGIT,ACTIVE
      EXTERNAL LENGTH,DIGIT
C
      LENS=LENGTH(STRING,N,1)
      DELL=1
C
C     REMOVE TRAILING '0'ES
C
      IIZERO=LENS+1
      ACTIVE=.FALSE.
      II=0
      DO WHILE(II.LE.LENS)
         II=II+1
         IF ((II.LE.LENS).AND.
     &        (DIGIT(STRING(II:II)).OR.
     &        (STRING(II:II).EQ.'.'))) THEN
            IF (STRING(II:II).EQ.'0') THEN
               IF (ACTIVE.AND.IIZERO.GT.II) IIZERO=II
            ELSEIF(STRING(II:II).EQ.'.') THEN
               ACTIVE=.TRUE.
C     WE DO NOT WANT THE PROGRAM TO CONVERT REAL TO INTEGERS, I.E. 11.0 => 11
C     IF (IIZERO.GT.II) IIZERO=II
            ELSE
               IIZERO=LENS+1
            ENDIF
         ELSE
            IF (ACTIVE.AND.IIZERO.LT.II) THEN
C     REMOVE EVERYTHING FROM IIZERO TO II-1
               DELL=II-IIZERO
               CALL SUBSTR(STRING,N,BUFF,0,IIZERO,DELL)
               II=IIZERO+1
               LENS=LENGTH(STRING,N,LENS-DELL)
               IIZERO=LENS+1
            ENDIF
            ACTIVE=.FALSE.
         ENDIF
      ENDDO
C
      LENS=LENGTH(STRING,N,LENS)
      CALL CHOP0(STRING,LENS)
C
      RETURN
      END
#__file: 'which.F' 0100664    **DO NOT DELETE**
      BLOCK DATA BDWHICH
      IMPLICIT NONE
      LOGICAL FIRST
      COMMON /CCWHICH/FIRST
      DATA FIRST/.TRUE./
      END BLOCK DATA


      SUBROUTINE WHICH(CCHR,CODE)
C     ***********************************************************************
C     +                                                                     *
C     FINDS OUT IF STRING CONTAINS A SPECIAL CHARACTER AT POS               *
C     +                                                                     *
C     CCHR (C*1)                                                            *
CO    CODE (I*4) = OBJECT TYPE IDENTIFIER                                   *
C     +     =0 UNIDENTIFIED                                                 *
C     +     =1 ' '                                                          *
C     +     =2 '^'                                                          *
C     +     =3 '*'                                                          *
C     +     =4 '/'                                                          *
C     +     =5 '+'                                                          *
C     +     =6 '-'                                                          *
C     +     =7 '='                                                          *
C     +     =8 '&'                                                          *
C     +     =9 '|'                                                          *
C     +     =10'>'                                                          *
C     +     =11'<'                                                          *
C     +     =12'!'                                                          *
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
C
      IMPLICIT NONE
C
      CHARACTER*1 CCHR,C1
      INTEGER CODE
C
      INTEGER MAXOBJ,II
      PARAMETER (MAXOBJ=13)
      CHARACTER*1 OBJECTS(MAXOBJ)
      LOGICAL FIRST,FOUND
      COMMON /CCWHICH/ FIRST
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'WHICH'/
C
      IF (FIRST) THEN
         C1=CHAR(0)
         OBJECTS(1)=' '
         OBJECTS(2)='^'
         OBJECTS(3)='*'
         OBJECTS(4)='/'
         OBJECTS(5)='+'
         OBJECTS(6)='-'
         OBJECTS(7)='='
         OBJECTS(8)='&'
         OBJECTS(9)='|'
         OBJECTS(10)='>'
         OBJECTS(11)='<'
         OBJECTS(12)='!'
         OBJECTS(13)=C1
         FIRST=.FALSE.
      ENDIF
C
C     SEARCH IN STRING
C
      CODE=0
      II=1
      FOUND=.FALSE.
      DO WHILE (II.LE.MAXOBJ .AND..NOT.FOUND)
         IF (OBJECTS(II).EQ.CCHR) THEN
            IF (.NOT.FOUND) FOUND=.TRUE.
            IF (FOUND) CODE=II
         ELSE
            II=II+1
         ENDIF
      ENDDO
C
      RETURN
      END
#__file: 'wobject.F' 0100664    **DO NOT DELETE**
      SUBROUTINE WOBJECT(STRING,N,START,POS,CODE)
C
C     RETURNS THE 'WHOLE' OBJECT (INCLUDED BRACKETS)
C
      IMPLICIT NONE
C
      INTEGER N
      CHARACTER*(*) STRING
      INTEGER START,POS(2),CODE
C
      INTEGER ICODE
      CHARACTER*16 MYNAME
      DATA MYNAME /'OBJECT'/
C
      ICODE=CODE
      CALL OBJECT(STRING,N,START,POS,ICODE)
C
      IF (CODE.EQ.1) THEN
         IF (POS(1).GT.1) THEN
            IF (STRING(POS(1)-1:POS(1)-1).EQ.'('
     &           .OR.STRING(POS(1)-1:POS(1)-1)
     &           .EQ.'[') THEN
               POS(1)=POS(1)-1
               IF (POS(1).GT.1) THEN
                  IF(STRING(POS(1)-1:POS(1)-1).EQ.'-'
     &                 .OR.STRING(POS(1)-1:POS(1)-1)
     &                 .EQ.'+') THEN
                     POS(1)=POS(1)-1
                  ENDIF
               ENDIF
            ELSEIF((STRING(POS(1)-1:POS(1)-1).EQ.'-'
     &              .OR.STRING(POS(1)-1:POS(1)-1)
     &              .EQ.'+').AND..NOT.
     &              (STRING(POS(1):POS(1)).EQ.'-'
     &              .OR.STRING(POS(1):POS(1))
     &              .EQ.'+')) THEN
               POS(1)=POS(1)-1
            ENDIF
         ENDIF
         POS(2)=START
      ELSEIF (CODE.EQ.-1) THEN
         POS(1)=START
         IF (POS(2).LT.N) THEN
            IF (STRING(POS(2)+1:POS(2)+1).EQ.')'
     &           .OR.STRING(POS(2)+1:POS(2)+1)
     &           .EQ.']') POS(2)=POS(2)+1
         ENDIF
      ENDIF
C
      CODE=ICODE
C
      RETURN
      END
