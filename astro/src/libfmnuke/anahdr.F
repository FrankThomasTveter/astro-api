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

