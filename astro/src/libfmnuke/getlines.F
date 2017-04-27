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

      
