      subroutine astroShort(lat10,lon10,hgt10,day10,dat10,off6,   ! query string
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250)             ! output string
C     
      implicit none
C     
      character*10 :: lat10,lon10,hgt10,day10,dat10
      integer :: maxline
      integer :: nline
      integer :: lenline(maxline)
      character*250 :: line250(maxline)
C     
      real :: lat, lon, hgt, val
      character*6 :: off6
      real :: date
      integer :: days
C     
      integer :: lens,lenx
      integer, external :: length
      integer :: yy1,mm1,dd1,hh1,mi1
      real :: sec1, s2000, t2000
      integer :: mode,ii,irc
      integer :: istart,istop,nval
      logical :: debug, bdeb, bdone
      data bdeb /.false./
C     
C     bdeb=.true.
      call chop0(lat10,10)
      lenx=length(lat10,10,3)
      read(lat10(1:lenx),*,iostat=irc) lat; if (irc.ne.0) lat=0.0D0
      call chop0(lon10,10)
      lenx=length(lon10,10,3)
      read(lon10(1:lenx),*,iostat=irc) lon; if (irc.ne.0) lon=0.0D0
      call chop0(hgt10,10)
      lenx=length(hgt10,10,3)
      read(hgt10(1:lenx),*,iostat=irc) hgt; if (irc.ne.0) hgt=0.0D0
      call chop0(day10,10)
      lenx=length(day10,10,3)
      read(day10(1:lenx),*,iostat=irc) days; if (irc.ne.0) days=1
      call chop0(dat10,10)
      lenx=length(dat10,10,3)
      read(dat10(1:lenx),'(I4,X,I2,X,I2)',iostat=irc)yy1,mm1,dd1
      if (irc.ne.0) then
         yy1=2019
         mm1=1
         dd1=1
      end if
C
      call date2jd(date,yy1,mm1,dd1,0,0,0.0D0)
C     write(*,*) "Input:",lat10,lon10,hgt10,day10,dat10,off6,date
C
      nline=0
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')"Content-Type: text/xml;"
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')" "
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')
     &     '<?xml version="1.0" encoding="utf-8"?>'
      lenline(nline)=len(trim(line250(nline)))

      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')
     &     '<astrodata xmlns:xsi='//
     &     '"http://www.w3.org/2001/XMLSchema-instance"'//
     &     ' xsi:noNamespaceSchemaLocation='//
     &     '"http://schema.api.met.no/schemas/astrodata-2.0.xsd">'
      lenline(nline)=len(trim(line250(nline)))

      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')
     &     '<meta licenseurl="https://api.met.no/license_data.html"/>'
      lenline(nline)=len(trim(line250(nline)))

      call execute_event(lat,lon,hgt,date,off6,days,
     &     maxline,nline,line250,lenline)

      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')'</astrodata>'
      lenline(nline)=len(trim(line250(nline)))
C     
      contains
C     
      subroutine execute_event(lat,lon,hgt,date,off6,days,
     &     imaxline,inline,iline250,ilenline)
      implicit none
      integer :: imaxline
      integer :: inline
      integer :: ilenline(maxline)
      character*250 :: iline250(maxline)
C     
      integer, parameter :: maxline=100
      integer :: nline
      integer :: lenline(maxline)
      character*250 :: line250(maxline)
      real :: jdline(maxline)
      integer :: indline(maxline)
C     
      real :: lat, lon, hgt, val
      character*6 :: off6
      real :: date, offset
      integer :: days
C     
      real tstartJD             ! start time (in julianDay)
      integer searchCode        ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
      real tstopJD              ! report all events end time (in julianDay)
      integer eventId           ! requested event id (SEE TABLE BELOW)
      integer meventVal         ! number of event input data (SEE TABLE ABOVE)
      parameter (meventVal=5)
      integer neventVal         ! number of event input data (SEE TABLE ABOVE)
      real eventVal(max(1,meventVal)) ! event input data (SEE TABLE ABOVE)
      integer maxrep            ! maximum number of output reports
      parameter (maxrep=1000)
      integer nrep              ! number of output reports
      real  repJD(maxrep)       ! time of output report (in julianDay)
      integer repId(maxrep)     ! output report identification (SEE TABLE ABOVE)
      real repVal(maxrep)       ! output report value (SEE TABLE ABOVE)
      character*250 rep250(maxrep) ! output report string (redundant)
      integer secdec            ! number of second decimals in output report string 
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
C
      character*50 :: x50, y50, z50, t50
      integer :: lenx, leny, lenz, lent
C
      integer, external :: length
      integer :: yy1,mm1,dd1,hh1,mi1
      integer :: newnn

      character*250 :: s250
      character*50 :: val50
      integer :: ii,jj,dd,lens
      character*25 :: dat25
C     
      secdec=0
      read(off6(2:6),'(I2,X,I2)',iostat=irc) hh1,mi1
      if (irc.ne.0) then
         irc=0
         offset=0.0D0
         off6="+00:00"
      else
         offset=hh1/24.0D0 + mi1/1440.0D0
         if (off6(1:1).eq."-") then
            offset=-offset
         end if
      end if
C
      x50=getVal50(lat,lenx)
      y50=getVal50(lon,leny)
      z50=getVal50(hgt,lenz)
      write(s250,80)x50(1:lenx),y50(1:leny),z50(1:lenz)
      call chop0(s250,250)
      lens=length(s250,250,30)
      inline=min(imaxline,inline+1)
      write(iline250(inline),'(A)') '  '//s250(1:lens)
      ilenline(inline)=len(trim(iline250(inline)))
C
      searchCode=2
      neventVal=3
      eventVal(1)=lat
      eventVal(2)=lon
      eventVal(3)=hgt
      do dd=1,days
C
         tstartjd=date-offset+real(dd-1)
C
         s2000=date+real(dd-1)
         call getDate(real(int(s2000)),s2000,yy1,mm1,dd1,hh1,mi1,sec1)
         write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C
         write(s250,90)dat25(1:10)
         call chop0(s250,250)
         lens=length(s250,250,30)
         inline=min(imaxline,inline+1)
         write(iline250(inline),'(A)') '  '//s250(1:lens)
         ilenline(inline)=len(trim(iline250(inline)))
C     
         nline=0
C
         if (dd.eq.1) then
C
C     100 - REPORT LOCAL INITIAL MOON STATE
         eventId=100
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <!-- moon stuff -->
C     <xsl:template match="/*/astro:Event[@Id='100']">
C     <moonphase time="{astro:Report[@repId='102']/@time}"
C     value="{astro:Report[@repId='102']/@repVal}"
C     desc="{substring(astro:Report[@repId='102']/@hint,21)}"/>
C     <moonshadow time="{astro:Report[@repId='103']/@time}"
C     elevation="{astro:Report[@repId='104']/@repVal}"
C     azimuth="{astro:Report[@repId='103']/@repVal}"
C     desc="{substring(astro:Report[@repId='103']/@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep-2
            if (repId(ii+2).eq.102) then ! phase
C     ! write moon phase information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               x50=getVal50(repVal(ii+2),lenx)
               write(s250,100)dat25,x50(1:lenx),
     &              rep250(ii)(21:lens)
               ! write(50,*)":"//rep250(ii+2)(1:lens)//":"
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000-2.0D-5
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
C     
               if (repId(ii+3).eq.103 .and. ! clock angle
     &              repId(ii+4).eq.104) then ! incidence
C     ! write moon shadow information
                  call chop0(rep250(ii),250)
                  lens=length(rep250(ii),250,20)
                  x50=getVal50(repval(ii+3),lenx)
                  y50=getVal50(repval(ii+4),leny)
                  write(s250,101)dat25,x50(1:lenx),y50(1:leny),
     &                 rep250(ii)(21:lens)
                  call chop0(s250,250)
                  lens=length(s250,250,30)
                  nline=min(maxline,nline+1)
                  jdline(nline)=t2000-1.0D-5
                  write(line250(nline),'(A)') '   '//s250(1:lens)
                  lenline(nline)=len(trim(line250(nline)))
               end if
            end if
         end do
         end if

 100  FORMAT('<moonphase time="',A25,'" value="',A,
     &     '" desc="',A,'"/>')
 101  FORMAT('<moonshadow time="',A25,'" azimuth="',A,
     &     '" elevation="',A,'" desc="',A,'"/>')
C     110 - REPORT LOCAL TC EF MOON POSITION AT TIME INCREMENT
         eventId=110
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='110']/astro:Report">
C     <moonposition time="{@time}"
C     elevation="{@repVal}"
C     azimuth="{following-sibling::astro:Report[@repId='111' and @time = current()/@time]/@repVal}"
C     range="{following-sibling::astro:Report[@repId='112' and @time = current()/@time]/@repVal}"
C     phase="{following-sibling::astro:Report[@repId='115' and @time = current()/@time]/@repVal}"
C     desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep-5
            if (repId(ii).eq.110) then ! elevation
C     ! write moon phase information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               if (repId(ii+1).eq.111 .and. ! azimuth
     &              repId(ii+2).eq.112 .and. ! range
     &              repId(ii+5).eq.115) then ! phase
                  x50=getVal50(repval(ii),lenx)
                  y50=getVal50(repval(ii+1),leny)
                  z50=getVal50(repval(ii+2),lenz)
                  t50=getVal50(repval(ii+5),lent)
                  write(s250,110)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz),t50(1:lent),rep250(ii)(21:lens)
                  call chop0(s250,250)
                  lens=length(s250,250,30)
                  nline=min(maxline,nline+1)
                  jdline(nline)=t2000
                  write(line250(nline),'(A)') '   '//s250(1:lens)
                  lenline(nline)=len(trim(line250(nline)))
               end if
            end if
         end do
 110  FORMAT('<moonposition time="',A25,'" elevation="',A,
     &     '" azimuth="',A,
     &     '" range="',A,
     &     '" phase="',A,'" desc="',A,'"/>')
C
C     600 - DETECT LOCAL DIURNAL SUN RISE
         eventId=600
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='600']/astro:Report">
C     <sunrise time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.600) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,600)dat25,rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 600  FORMAT('<sunrise time="',A25,'" desc="',A,'"/>')

C     610 - DETECT LOCAL DIURNAL SUN SET
         eventId=610
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='610']/astro:Report">
C     <sunset time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.610) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,610)dat25,rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 610  FORMAT('<sunset time="',A25,'" desc="',A,'"/>')

C     620 - DETECT LOCAL DIURNAL MAXIMUM SOLAR ELEVATION
         eventId=620
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='620']/astro:Report">
C     <solarnoon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.620) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C               write(*,*)'REP:',ii,repjd(ii),tstartJD,tstopJD,
C     &              offset,rep250(ii)(21:lens)
               x50=getVal50(repval(ii),lenx)
               write(s250,620)dat25,x50(1:lenx),rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 620  FORMAT('<solarnoon time="',A25,
     &        '" elevation="',A,'" desc="',A,'"/>')

C     630 - DETECT LOCAL DIURNAL MINIMUM SOLAR ELEVATION
         eventId=630
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='630']/astro:Report">
C     <solarmidnight time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.630) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getVal50(repval(ii),lenx)
               write(s250,630)dat25,x50(1:lenx),rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 630  FORMAT('<solarmidnight time="',A25,'" elevation="',
     &        A,'" desc="',A,'"/>')

C     800 - DETECT LOCAL DIURNAL MOON RISE
         eventId=800
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='800']/astro:Report">
C     <moonrise time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.800) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,800)dat25,rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 800  FORMAT('<moonrise time="',A25,'" desc="',A,'"/>')

C     810 - DETECT LOCAL DIURNAL MOON SET
         eventId=810
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='810']/astro:Report">
C     <moonset time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.810) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,810)dat25,rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 810  FORMAT('<moonset time="',A25,'" desc="',A,'"/>')

C     820 - DETECT LOCAL DIURNAL MAXIMUM MOON ELEVATION
         eventId=820
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='820']/astro:Report">
C     <high_moon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.820) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getVal50(repval(ii),lenx)
               write(s250,820)dat25,x50(1:lenx),rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 820  FORMAT('<high_moon time="',A25,
     &        '" elevation="',A,'" desc="',A,'"/>')

C     830 - DETECT LOCAL DIURNAL MINIMUM MOON ELEVATION
         eventId=830
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='830']/astro:Report">
C     <low_moon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.830) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getVal50(repval(ii),lenx)
               write(s250,830)dat25,x50(1:lenx),rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 830  FORMAT('<low_moon time="',A25,
     &        '" elevation="',A,'" desc="',A,'"/>')

C     900 - DETECT LOCAL POLAR SUN DAY START
         eventId=900
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     
C     <!-- polar stuff -->
C     <xsl:template match="/*/astro:Event[@Id='900']/astro:Report">
C     <polardaystart time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.900) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,900)dat25,rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 900  FORMAT('<polardaystart time="',A25,'" desc="',A,'"/>')

C     910 - DETECT LOCAL POLAR SUN DAY STOP
         eventId=910
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='910']/astro:Report">
C     <polardayend time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.910) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,910)dat25,rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 910  FORMAT('<polardayend time="',A25,'" desc="',A,'"/>')

C     920 - DETECT LOCAL POLAR SUN NIGHT START
         eventId=920
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='920']/astro:Report">
C     <polarnightstart time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.920) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,920)dat25,rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 920  FORMAT('<polarnightstart time="',A25,'" desc="',A,'"/>')

C     940 - DETECT LOCAL POLAR SUN NIGHT STOP
         eventId=940
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchCode,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &        tstopJD,          ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep,             ! number of output reports
     &        repJD,            ! time of output report (in julianDay)
     &        repId,            ! output report identification (SEE TABLE BELOW)
     &        repVal,           ! output report value (SEE TABLE BELOW)
     &        rep250,           ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error description
     &        irc)              ! error return code (0=ok)
C     <xsl:template match="/*/astro:Event[@Id='930']/astro:Report">
C     <polarnightend time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.930) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,930)dat25,rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 930  FORMAT('<polarnightend time="',A25,'" desc="',A,'"/>')
C
C     push data to output arrays
C
      do ii=1,nline
         indline(ii)=ii
      end do
      call heapsort1(nline,jdline,newnn,nline,indline,1.0D-5,.false.)

      do ii=1,newnn
         inline=min(imaxline,inline+1)
         iline250(inline)=line250(indline(ii))
         ilenline(inline)=lenline(indline(ii))
      end do
C     close time tag
         write(s250,91)
         call chop0(s250,250)
         lens=length(s250,250,30)
         inline=min(imaxline,inline+1)
         write(iline250(inline),'(A)') '  '//s250(1:lens)
         ilenline(inline)=len(trim(iline250(inline)))
      end do
C
C     add a final moonposition
C
      tstartjd=date-offset+real(days)
      s2000=date+real(days)
      call getDate(real(int(s2000)),s2000,yy1,mm1,dd1,hh1,mi1,sec1)
      write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C     
      write(s250,90)dat25(1:10)
      call chop0(s250,250)
      lens=length(s250,250,30)
      inline=min(imaxline,inline+1)
      write(iline250(inline),'(A)') '  '//s250(1:lens)
      ilenline(inline)=len(trim(iline250(inline)))
      

C     110 - REPORT LOCAL TC EF MOON POSITION AT TIME INCREMENT
      eventId=110
      nrep=0
      irc=0
      
      tstopjd=tstartjd+1.0D0
      call astroEvent(
     &     tstartJD,            ! start time (in julianDay)
     &     searchCode,          ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &     tstopJD,             ! report all events end time (in julianDay)
     &     eventId,             ! requested event id (SEE TABLE BELOW)
     &     neventVal,           ! number of event input data (SEE TABLE BELOW)
     &     eventVal,            ! event input data (SEE TABLE BELOW)
     &     maxrep,              ! maximum number of output reports
     &     nrep,                ! number of output reports
     &     repJD,               ! time of output report (in julianDay)
     &     repId,               ! output report identification (SEE TABLE BELOW)
     &     repVal,              ! output report value (SEE TABLE BELOW)
     &     rep250,              ! output report string (redundant description)
     &     secdec,              ! number of second decimals used in output report string 
     &     crc250,              ! error description
     &     irc)                 ! error return code (0=ok)

C     <xsl:template match="/*/astro:Event[@Id='110']/astro:Report">
C     <moonposition time="{@time}"
C     elevation="{@repVal}"
C     azimuth="{following-sibling::astro:Report[@repId='111' and @time = current()/@time]/@repVal}"
C     range="{following-sibling::astro:Report[@repId='112' and @time = current()/@time]/@repVal}"
C     phase="{following-sibling::astro:Report[@repId='115' and @time = current()/@time]/@repVal}"
C     desc="{substring(@hint,21)}"/>
C     </xsl:template>
      do ii=1,nrep-5
         if (repId(ii).eq.110) then ! elevation
C     ! write moon phase information
            t2000=repJD(ii)+offset
            call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
            call chop0(rep250(ii),250)
            lens=length(rep250(ii),250,20)
C            write(*,*) "times:",tstartjd, t2000,offset,dd1,hh1
            write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
            if (repId(ii+1).eq.111 .and. ! azimuth
     &           repId(ii+2).eq.112 .and. ! range
     &           repId(ii+5).eq.115) then ! phase
               x50=getVal50(repval(ii),lenx)
               y50=getVal50(repval(ii+1),leny)
               z50=getVal50(repval(ii+2),lenz)
               t50=getVal50(repval(ii+5),lent)
               write(s250,110)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz),t50(1:lent),rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               inline=min(imaxline,inline+1)
               write(iline250(inline),'(A)') '   '//s250(1:lens)
               ilenline(inline)=len(trim(iline250(inline)))
            end if
         end if
      end do
C     close time tag
      write(s250,91)
      call chop0(s250,250)
      lens=length(s250,250,30)
      inline=min(imaxline,inline+1)
      write(iline250(inline),'(A)') '  '//s250(1:lens)
      ilenline(inline)=len(trim(iline250(inline)))
C     close location tag
      write(s250,81)
      call chop0(s250,250)
      lens=length(s250,250,30)
      inline=min(imaxline,inline+1)
      write(iline250(inline),'(A)') '  '//s250(1:lens)
      ilenline(inline)=len(trim(iline250(inline)))

 80   FORMAT('<location latitude="',A
     &     ,'" longitude="',A,'" height="',A,'">')
 81   FORMAT('</location>')
 90   FORMAT('<time date="',A10,'">')
 91   FORMAT('</time>')


 999  FORMAT(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,A6) ! 19
      end subroutine execute_event
C    
      character*50 function getVal50(val,lenv)
      implicit none
      character*50 :: buff
      integer :: j1,j2,kk
      real val
      integer :: lenv
      integer, external :: length
      if (abs(val-nint(val)).lt.1.0D-10) then
         write(getVal50,*)nint(val)
      else
         write(buff,'(F30.10)') val
         j1=0
         j2=30
         LOOP: do kk=30,1,-1
            if (j1.eq.0.and.buff(kk:kk).eq.'.') then
               j1=kk
               exit LOOP
            else if (j1.eq.0.and.j2.eq.30.and.
     &              buff(kk:kk).ne.'0') then
               j2=kk
            end if
         end do LOOP
         if (j1.ne.1.and.j1.eq.j2) then
            getVal50=buff(1:j1-1)
         else
            getVal50=buff(1:j2)
         end if
      end if
      call chop0(getVal50,50)
      lenv=length(getVal50,50,10)
      return
      end function getVal50


      subroutine getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
      real :: s2000,t2000
      integer :: yy1,mm1,dd1,hh1,mi1
      real :: sec1
      integer :: lens
      integer, external :: length
      sec1=(t2000-s2000)*86400.0D0
      sec1=min(86400.0D0-1.0D0,max(0.0D0,real(nint(sec1))))
      t2000=s2000+(sec1/86400.0D0)
      call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
      return
      end subroutine getDate
 
      end subroutine astroShort
