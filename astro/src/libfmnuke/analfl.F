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
