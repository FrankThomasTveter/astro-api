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
