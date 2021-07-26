      subroutine astroSmall(lat10,lon10,hgt10,day10,dat10,off6,   ! query string
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
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_
      logical sup
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
     &     '<astroData xmlns:xsi='//
     &     '"http://www.w3.org/2001/XMLSchema-instance"'//
     &     ' xsi:noNamespaceSchemaLocation='//
     &     '"http://schema.api.met.no/schemas/astrodata-2.0.xsd">'
      lenline(nline)=len(trim(line250(nline)))

      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')
     &     ' <meta licenseurl="https://api.met.no/license_data.html"/>'
      lenline(nline)=len(trim(line250(nline)))

      call execute_event(lat,lon,hgt,date,off6,days,
     &     maxline,nline,line250,lenline)

      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')'</astroData>'
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
      write(iline250(inline),'(A)') ' '//s250(1:lens)
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
C     120 - REPORT LOCAL INITIAL SUN STATE
         eventId=120
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
         do ii=1,nrep-2
            if (repId(ii).eq.120 .and. ! sun state
     &           repId(ii+1).eq.121 .and. ! polar effect
     &           repId(ii+2).eq.122) then ! elevation
C     ! write sun information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               if (nint(repVal(ii)).eq.1) then
                  y50="true"
               else
                  y50="false"
               end if
               leny=len(trim(y50))
               if (nint(repVal(ii+1)).eq.1) then
                  z50="day"
               else if (nint(repVal(ii+1)).eq.0) then
                  z50="none"
               else if (nint(repVal(ii+1)).eq.-1) then
                  z50="night"
               end if
               lenz=len(trim(z50))
               x50=getValN50(repval(ii+2),lenx,4)
               write(s250,120)dat25,x50(1:lenx),
     &              y50(1:leny),z50(1:lenz)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000-2.0D-5
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 120     FORMAT('<sun time="',A25,'" elevation="',A,
     &        '" visible="',A,'" polar="',A,'"/>')
         if (dd.eq.1) then
C     
C     100 - REPORT LOCAL INITIAL MOON STATE
            eventId=100
            nrep=0
            irc=0
            tstopjd=tstartjd+1.0D0
            call astroEvent(
     &           tstartJD,      ! start time (in julianDay)
     &           searchCode,    ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &           tstopJD,       ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep,          ! number of output reports
     &           repJD,         ! time of output report (in julianDay)
     &           repId,         ! output report identification (SEE TABLE BELOW)
     &           repVal,        ! output report value (SEE TABLE BELOW)
     &           rep250,        ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error description
     &           irc)           ! error return code (0=ok)
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
               if (repId(ii).eq.102 .and.
     &              repId(ii+1).eq.103 .and.
     &              repId(ii+2).eq.104) then ! phase
C     ! write moon shadow information
                  call chop0(rep250(ii),250)
                  lens=length(rep250(ii+1),250,20)
                  x50=getValN50(repval(ii+2),lenx,4)
                  y50=getValN50(repval(ii+1),leny,4)
                  write(s250,100)dat25,x50(1:lenx),y50(1:leny) ! rep250(ii+1)(21:lens)
                  call chop0(s250,250)
                  lens=length(s250,250,30)
                  nline=min(maxline,nline+1)
                  jdline(nline)=t2000-1.0D-5
                  write(line250(nline),'(A)') '   '//s250(1:lens)
                  lenline(nline)=len(trim(line250(nline)))
               end if
            end do
         end if
 100     FORMAT('<moonShadow time="',A25,'" iangle="',A,
     &        '" cangle="',A,'"/>')
         
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
            if (repId(ii).eq.110.and. ! elevation
     &           repId(ii+1).eq.111 .and. ! azimuth
     &           repId(ii+2).eq.112 .and. ! range
     &           repId(ii+5).eq.115) then ! phase
C     ! write moon phase information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+2),lenz,1)
               t50=getValn50(repval(ii+5),lent,1)
               write(s250,110)dat25,x50(1:lenx),y50(1:leny),
     &              z50(1:lenz),t50(1:lent) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 110  FORMAT('<moon time="',A25,'" elevation="',A,
     &     '" azimuth="',A,
     &     '" range="',A,
     &     '" phase="',A,'"/>')

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
         do ii=1,nrep-2
            if (repId(ii).eq.600.and. ! event
     &           repId(ii+1).eq.601) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               write(s250,600)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 600  FORMAT('<sunRise time="',A25,'" azimuth="',A,'"/>')

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
         do ii=1,nrep-2
            if (repId(ii).eq.610.and. ! event
     &           repId(ii+1).eq.611) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               write(s250,610)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 610  FORMAT('<sunSet time="',A25,'" azimuth="',A,'"/>')

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
         do ii=1,nrep-2
            if (repId(ii).eq.620.and. ! upper limb elevation
     &           repId(ii+1).eq.621.and. ! azimuth
     &           repId(ii+2).eq.622) then ! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C               write(*,*)'REP:',ii,repjd(ii),tstartJD,tstopJD,
C     &              offset ! rep250(ii)(21:lens)
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               sup=(repval(ii).gt.-refraction)
               if (sup) then
                  write(s250,620)dat25,x50(1:lenx),y50(1:leny) !rep250(ii)(21:lens)
               else
                  write(s250,621)dat25,x50(1:lenx),y50(1:leny) !rep250(ii)(21:lens)
               end if
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 620  FORMAT('<sunHigh time="',A25,'" elevation="',A,
     &        '" azimuth="',A,'" visible="true"/>')
 621  FORMAT('<sunHigh time="',A25,'" elevation="',A,
     &        '" azimuth="',A,'" visible="false"/>')

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
         do ii=1,nrep-2
            if (repId(ii).eq.630.and. ! upper limb elevation
     &           repId(ii+1).eq.631.and. ! azimuth
     &           repId(ii+2).eq.632) then! elevation
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               sup=(repval(ii).gt.-refraction)
               if (sup) then
                  write(s250,630)dat25,x50(1:lenx),y50(1:leny) !rep250(ii)(21:lens)
               else
                  write(s250,631)dat25,x50(1:lenx),y50(1:leny) !rep250(ii)(21:lens)
               end if
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 630  FORMAT('<sunLow time="',A25,'" elevation="',
     &        A,'" azimuth="',A,'" visible="true"/>')
 631  FORMAT('<sunLow time="',A25,'" elevation="',
     &        A,'" azimuth="',A,'" visible="false"/>')

C
C     660 - DETECT LOCAL DIURNAL NAUTICAL TWILIGHT START
         eventId=660
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
         do ii=1,nrep-2
            if (repId(ii).eq.660.and. ! event
     &           repId(ii+1).eq.661) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               write(s250,660)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 660  FORMAT('<twilightNauticalStart time="',A25,'" azimuth="',A,'"/>')

C     670 - DETECT LOCAL DIURNAL NAUTICAL TWILIGHT STOP
         eventId=670
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
         do ii=1,nrep-2
            if (repId(ii).eq.670.and. ! event
     &           repId(ii+1).eq.671) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               write(s250,670)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 670  FORMAT('<twilightNauticalEnd time="',A25,'" azimuth="',A,'"/>')
C
C     680 - DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT START
         eventId=680
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
         do ii=1,nrep-2
            if (repId(ii).eq.680.and. ! event
     &           repId(ii+1).eq.681) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               write(s250,680)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 680     FORMAT('<twilightAstronomicalStart time="',A25,
     &        '" azimuth="',A,'"/>')
         
C     690 - DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT STOP
         eventId=690
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
         do ii=1,nrep-2
            if (repId(ii).eq.690.and. ! event
     &           repId(ii+1).eq.691) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               write(s250,690)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 690     FORMAT('<twilightAstronomicalEnd time="',A25,
     &        '" azimuth="',A,'"/>')
C     
C     700 - DETECT LOCAL DIURNAL NIGHT START
         eventId=700
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
         do ii=1,nrep-2
            if (repId(ii).eq.700.and. ! event
     &           repId(ii+1).eq.701) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               write(s250,700)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 700     FORMAT('<nightStart time="',A25,'" azimuth="',A,'"/>')

C     710 - DETECT LOCAL DIURNAL NIGHT STOP
         eventId=710
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
         do ii=1,nrep-2
            if (repId(ii).eq.710.and. ! event
     &           repId(ii+1).eq.711) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               write(s250,710)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 710     FORMAT('<nightStop time="',A25,'" azimuth="',A,'"/>')

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
         do ii=1,nrep-3
            if (repId(ii).eq.800.and. ! event
     &           repId(ii+1).eq.803 .and. ! azimuth
     &           repId(ii+3).eq.805) then ! illumination
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+3),lenz,1)
               write(s250,800)dat25,y50(1:leny),
     &              z50(1:lenz) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 800  FORMAT('<moonRise time="',A25,'" azimuth="',A,
     &        '" illumination="',A,'"/>')

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
            if (repId(ii).eq.810.and. ! event
     &           repId(ii+1).eq.813 .and. ! azimuth
     &           repId(ii+3).eq.815) then ! illumination
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+3),lenz,1)
               write(s250,810)dat25,y50(1:leny),
     &              z50(1:lenz) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 810  FORMAT('<moonSet time="',A25,'" azimuth="',A,
     &        '" illumination="',A,'"/>')

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
         do ii=1,nrep-3
            if (repId(ii).eq.820 .and. ! upper limb elevation
     &           repId(ii+1).eq.823 .and. ! azimuth
     &           repId(ii+2).eq.824 .and. ! elevation
     &           repId(ii+3).eq.825) then ! illumination
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+3),lenz,1)
               sup=(repval(ii).gt.-refraction)
               if (sup) then
                  write(s250,820)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz) ! rep250(ii)(21:lens)
               else
                  write(s250,821)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz) ! rep250(ii)(21:lens)
               end if
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 820  FORMAT('<moonHigh time="',A25,'" elevation="',A,
     &        '" azimuth="',A,'" illumination="',A,
     &        '" visible="true"/>')
 821  FORMAT('<moonHigh time="',A25,'" elevation="',A,
     &        '" azimuth="',A,'" illumination="',A,
     &        '" visible="false"/>')

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
         do ii=1,nrep-3
            if (repId(ii).eq.830 .and. ! upper limb elevation
     &           repId(ii+1).eq.833 .and. ! azimuth
     &           repId(ii+2).eq.834 .and. ! elevation
     &           repId(ii+3).eq.835) then ! illumination
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+3),lenz,1)
               sup=(repval(ii).gt.-refraction)
               if (sup) then
                  write(s250,830)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz) ! rep250(ii)(21:lens)
               else
                  write(s250,831)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz) ! rep250(ii)(21:lens)
               end if
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 830  FORMAT('<moonLow time="',A25,'" elevation="',A,
     &        '" azimuth="',A,'" illumination="',A,
     &        '" visible="true"/>')
 831  FORMAT('<moonLow time="',A25,'" elevation="',A,
     &        '" azimuth="',A,'" illumination="',A,
     &        '" visible="false"/>')

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
            if (repId(ii).eq.900) then ! event
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,900)dat25 ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 900  FORMAT('<polarDayStart time="',A25,'"/>')

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
            if (repId(ii).eq.910) then ! event
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,910)dat25 ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 910  FORMAT('<polarDayEnd time="',A25,'"/>')

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
            if (repId(ii).eq.920) then ! event
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,920)dat25 ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 920  FORMAT('<polarNightStart time="',A25,'"/>')

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
            if (repId(ii).eq.930) then ! event
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,930)dat25 ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 930  FORMAT('<polarNightEnd time="',A25,'"/>')
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
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+2),lenz,1)
               t50=getValn50(repval(ii+5),lent,1)
               write(s250,110)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz),t50(1:lent) ! rep250(ii)(21:lens)
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
      write(iline250(inline),'(A)') ' '//s250(1:lens)
      ilenline(inline)=len(trim(iline250(inline)))

 80   FORMAT('<location latitude="',A
     &     ,'" longitude="',A,'" height="',A,'">')
 81   FORMAT('</location>')
 90   FORMAT('<day date="',A10,'">')
 91   FORMAT('</day>')


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

      character*50 function getValN50(val,lenv,ndec)
      implicit none
      integer ndec
      character*50 :: buff
      integer :: j1,j2,kk
      real val
      character*20 fmt20
      integer :: lenv
      integer, external :: length
      write(fmt20,'("(F30.",I0,")")') ndec
      if (abs(val-nint(val)).lt.1.0D-10) then
         write(getValn50,*)nint(val)
      else
         write(buff,trim(fmt20)) val
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
            getValn50=buff(1:j1-1)
         else
            getValn50=buff(1:j2)
         end if
      end if
      call chop0(getValn50,50)
      lenv=length(getValn50,50,10)
      return
      end function getValN50


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
 
      end subroutine astroSmall
