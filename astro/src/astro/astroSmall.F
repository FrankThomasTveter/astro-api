      subroutine astroSmall(lat10,lon10,hgt10,day10,dat10,off6,eve10,   ! query string
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250)             ! output string
C     
      implicit none
C     
      character*10 :: lat10,lon10,hgt10,day10,dat10,eve10
      integer :: maxline
      integer :: nline
      integer :: lenline(maxline)
      character*250 :: line250(maxline)
C     
      real :: lat, lon, hgt, val
      character*6 :: off6
      real :: date
      integer :: days
      logical :: events
C     
      integer :: lens,lenx,lena,lenb,lenc,lend,lene
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
      call chop0(eve10,10)
      lenx=length(eve10,10,3)
      events=(lenx.ne.0)
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
     &     ' <meta licenseurl="https://api.met.no/license_data.html"/>'
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
      integer,parameter :: searchNext=1 ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
      integer,parameter :: searchUntil=2 ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
      character*50 :: r50, t50, u50, v50, w50, x50, y50, z50
      integer :: lenr, lent, lenu, lenv, lenw, lenx, leny, lenz
C
      integer, external :: length
      integer :: yy1,mm1,dd1,hh1,mi1
      integer :: newnn

      real rSun, rMoon, elevation, azimuth, range, upperlimb
      parameter ( rSun     = 682500.0D0 )
      parameter ( rMoon     =  1738.0D0 )

      character*250 :: s250
      character*50 :: val50
      integer :: ii,jj,dd,lens
      character*25 :: dat25,a25,b25,c25,d25,e25
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
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
         do ii=1,nrep-4
            if (repId(ii).eq.120 .and. ! elevation
     &           repId(ii+1).eq.121 .and. ! azimuth
     &           repId(ii+2).eq.122 .and. ! range
     &           repId(ii+3).eq.123 .and. ! visible
     &           repId(ii+4).eq.124) then ! polar effect
C     ! write sun information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               x50=getValN50(repval(ii),lenx,4)
               y50=getValN50(repval(ii+1),leny,4)
               z50=getValN50(repval(ii+2),lenz,1)
               if (nint(repVal(ii+3)).eq.1) then
                  u50="true"
               else
                  u50="false"
               end if
               lenu=len(trim(u50))
               if (nint(repVal(ii+4)).eq.1) then
                  v50="day"
               else if (nint(repVal(ii+4)).eq.0) then
                  v50="none"
               else if (nint(repVal(ii+4)).eq.-1) then
                  v50="night"
               end if
               lenv=len(trim(v50))
               write(s250,120)dat25,x50(1:lenx),y50(1:leny),z50(1:lenz),
     &              u50(1:lenu),v50(1:lenv)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000-2.0D-5
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 120     FORMAT('<sunposition time="',A25,'" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" visible="',A,'" polar="',A,'"/>')
C     
C     100 - REPORT LOCAL INITIAL MOON STATE
         eventId=100
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
C         write(*,*)"Reports:",nrep
C         do ii=1,nrep
C            write(*,*)"Report:",ii,repid(ii),repval(ii),rep250(ii)(1:90)
C         end do
         do ii=1,nrep-7
            if (repId(ii).eq.100 .and. ! elevation
     &           repId(ii+1).eq.101 .and. ! azimuth
     &           repId(ii+2).eq.102 .and. ! range
     &           repId(ii+3).eq.103 .and. ! visible
     &           repId(ii+4).eq.104 .and. ! polar effects
     &           repId(ii+5).eq.105 .and. ! incidence angle
     &           repId(ii+6).eq.106 .and. ! clock angle
     &           repId(ii+7).eq.107) then ! phase
C     ! write moon shadow information
               x50=getValN50(repval(ii),lenx,4) ! elevation
               y50=getValN50(repval(ii+1),leny,4) ! azimuth
               z50=getValN50(repval(ii+2),lenz,1) ! range
               r50=getValN50(repval(ii+6),lenr,1) ! clock
               t50=getValN50(repval(ii+5),lent,1) ! incidence
               w50=getValN50(repval(ii+7),lenw,1) ! phase
               call chop0(rep250(ii),250)
               lens=length(rep250(ii+3),250,20)
               if (nint(repVal(ii)).eq.1) then
                  u50="true"
               else
                  u50="false"
               end if
               lenu=len(trim(u50))
               if (nint(repVal(ii+4)).eq.1) then
                  v50="day"
               else if (nint(repVal(ii+4)).eq.0) then
                  v50="none"
               else if (nint(repVal(ii+4)).eq.-1) then
                  v50="night"
               end if
               lenv=len(trim(v50))
               write(s250,100)dat25,
     &              x50(1:lenx),y50(1:leny),z50(1:lenz),
     &              r50(1:lenr),t50(1:lent),
     &              w50(1:lenw),
     &              u50(1:lenu),v50(1:lenv)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000-1.0D-5
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 100     FORMAT('<moonposition time="',A25,'" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" cangle="',A,
     &        '" iangle="',A,
     &        '" phase="',A,
     &        '" visible="',A,'" polar="',A,'"/>')
C     
C     600 - DETECT LOCAL DIURNAL SUN RISE
         eventId=600
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.600) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               write(s250,600)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 600  FORMAT('<sunrise time="',A25,'" azimuth="',A,'"/>')

C     610 - DETECT LOCAL DIURNAL SUN SET
         eventId=610
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.610) then ! azimuth
C     ! write sunset information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               write(s250,610)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 610  FORMAT('<sunset time="',A25,'" azimuth="',A,'"/>')

C     620 - DETECT LOCAL DIURNAL MAXIMUM SOLAR ELEVATION
         eventId=620
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.620.and. ! elevation
     &           repId(ii+1).eq.621.and. ! azimuth
     &           repId(ii+2).eq.622) then ! range
C     ! write noon information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C               write(*,*)'REP:',ii,repjd(ii),tstartJD,tstopJD,
C     &              offset ! rep250(ii)(21:lens)
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+2),lenz,1)
               elevation=repval(ii)
               range=repval(ii+2)
               upperlimb=elevation + (rSun/range)
               sup=(upperlimb.gt.-refraction)
               if (sup) then
                  write(s250,620)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz) !rep250(ii)(21:lens)
               else
                  write(s250,621)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz) !rep250(ii)(21:lens)
               end if
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 620     FORMAT('<solarnoon time="',A25,
     &        '" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" visible="true"/>')
 621     FORMAT('<solarnoon time="',A25,
     &        '" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" visible="false"/>')

C     630 - DETECT LOCAL DIURNAL MINIMUM SOLAR ELEVATION
         eventId=630
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.630.and. ! elevation
     &           repId(ii+1).eq.631.and. ! azimuth
     &           repId(ii+2).eq.632) then! range
C     ! write midnight information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+2),lenz,1)
               elevation=repval(ii)
               range=repval(ii+2)
               upperlimb=elevation + (rSun/range)
               sup=(upperlimb.gt.-refraction)
               if (sup) then
                  write(s250,630)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz) !rep250(ii)(21:lens)
               else
                  write(s250,631)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz) !rep250(ii)(21:lens)
               end if
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 630     FORMAT('<solarmidnight time="',A25,
     &        '" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" visible="true"/>')
 631     FORMAT('<solarmidnight time="',A25,
     &        '" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" visible="false"/>')
         
C
#ifdef TWILIGHT
C     660 - DETECT LOCAL DIURNAL NAUTICAL TWILIGHT START
         eventId=660
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
C     <twilight time="{@time}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.660) then ! azimuth
C     ! write twilight information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               write(s250,660)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 660  FORMAT('<nauticalTwilightBegins time="',A25,'" azimuth="',A,'"/>')

C     670 - DETECT LOCAL DIURNAL NAUTICAL TWILIGHT STOP
         eventId=670
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.670) then ! azimuth
C     ! write twilight information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               write(s250,670)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 670  FORMAT('<nauticalTwilightEnds time="',A25,'" azimuth="',A,'"/>')
C
C     680 - DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT START
         eventId=680
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.680) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               write(s250,680)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 680     FORMAT('<astronomicalTwilightBegins time="',A25,
     &        '" azimuth="',A,'"/>')
         
C     690 - DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT STOP
         eventId=690
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.690) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               write(s250,690)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 690     FORMAT('<astronomicalTwilightEnds time="',A25,
     &        '" azimuth="',A,'"/>')
C     
C     700 - DETECT LOCAL DIURNAL NIGHT START
         eventId=700
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.700) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               write(s250,700)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 700     FORMAT('<nightBegins time="',A25,'" azimuth="',A,'"/>')

C     710 - DETECT LOCAL DIURNAL NIGHT STOP
         eventId=710
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.710) then ! azimuth
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               write(s250,710)dat25,y50(1:leny) ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 710     FORMAT('<nightEnds time="',A25,'" azimuth="',A,'"/>')
#endif

C     800 - DETECT LOCAL DIURNAL MOON RISE
         eventId=800
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
         do ii=1,nrep-2
            if (repId(ii).eq.800 .and. ! azimuth
     &           repId(ii+2).eq.802) then ! illumination
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               z50=getValn50(repval(ii+2),lenz,1)
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
 800  FORMAT('<moonrise time="',A25,'" azimuth="',A,
     &        '" illumination="',A,'"/>')

C     810 - DETECT LOCAL DIURNAL MOON SET
         eventId=810
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.810 .and. ! azimuth
     &           repId(ii+2).eq.812) then ! illumination
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               y50=getValn50(repval(ii),leny,4)
               z50=getValn50(repval(ii+2),lenz,1)
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
 810  FORMAT('<moonset time="',A25,'" azimuth="',A,
     &        '" illumination="',A,'"/>')

C     820 - DETECT LOCAL DIURNAL MAXIMUM MOON ELEVATION
         eventId=820
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.820 .and. ! elevation
     &           repId(ii+1).eq.821 .and. ! azimuth
     &           repId(ii+2).eq.822 .and. ! range
     &           repId(ii+3).eq.823) then ! illumination
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+2),lenz,1)
               u50=getValn50(repval(ii+3),lenu,1)
               elevation=repval(ii)
               range=repval(ii+2)
               upperlimb=elevation + (rMoon/range)
               sup=(upperlimb.gt.-refraction)
               if (sup) then
                  write(s250,820)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz), u50(1:lenu) ! rep250(ii)(21:lens)
               else
                  write(s250,821)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz), u50(1:lenu) ! rep250(ii)(21:lens)
               end if
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 820  FORMAT('<high_moon time="',A25,'" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" illumination="',A,
     &        '" visible="true"/>')
 821  FORMAT('<high_moon time="',A25,'" elevation="',A,
     &     '" azimuth="',A,
     &     '" range="',A,
     &     '" illumination="',A,
     &     '" visible="false"/>')
      
C     830 - DETECT LOCAL DIURNAL MINIMUM MOON ELEVATION
         eventId=830
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.830 .and. ! elevation
     &           repId(ii+1).eq.831 .and. ! azimuth
     &           repId(ii+2).eq.832 .and. ! range
     &           repId(ii+3).eq.833) then ! illumination
C     ! write sunrise information
               t2000=repJD(ii)+offset
               call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               x50=getValn50(repval(ii),lenx,4)
               y50=getValn50(repval(ii+1),leny,4)
               z50=getValn50(repval(ii+2),lenz,1)
               u50=getValn50(repval(ii+3),lenu,1)
               elevation=repval(ii)
               range=repval(ii+2)
               upperlimb=elevation + (rMoon/range)
               sup=(upperlimb.gt.-refraction)
               if (sup) then
                  write(s250,830)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz),u50(1:lenu) ! rep250(ii)(21:lens)
               else
                  write(s250,831)dat25,x50(1:lenx),y50(1:leny),
     &                 z50(1:lenz),u50(1:lenu) ! rep250(ii)(21:lens)
               end if
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '   '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 830     FORMAT('<low_moon time="',A25,'" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" illumination="',A,
     &        '" visible="true"/>')
 831     FORMAT('<low_moon time="',A25,'" elevation="',A,
     &        '" azimuth="',A,
     &        '" range="',A,
     &        '" illumination="',A,
     &        '" visible="false"/>')
         
C     900 - DETECT LOCAL POLAR SUN DAY START
         eventId=900
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
 900  FORMAT('<polarDayBegins time="',A25,'"/>')

C     910 - DETECT LOCAL POLAR SUN DAY STOP
         eventId=910
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
 910  FORMAT('<polarDayEnds time="',A25,'"/>')

C     920 - DETECT LOCAL POLAR SUN NIGHT START
         eventId=920
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
 920  FORMAT('<polarNightBegins time="',A25,'"/>')

C     940 - DETECT LOCAL POLAR SUN NIGHT STOP
         eventId=940
         nrep=0
         irc=0
         tstopjd=tstartjd+1.0D0
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchUntil,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
 930  FORMAT('<polarNightEnds time="',A25,'"/>')
C
C     push data to output arrays
C
      do ii=1,nline
         indline(ii)=ii
      end do
      call heapsort1(nline,jdline,newnn,nline,indline,1.0D-9,.false.)

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
C     add a final moonstate
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
      
C     
C     120 - REPORT LOCAL INITIAL SUN STATE
      eventId=120
      nrep=0
      irc=0
      tstopjd=tstartjd+1.0D0
      call astroEvent(
     &     tstartJD,            ! start time (in julianDay)
     &     searchUntil,          ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
      do ii=1,nrep-4
         if (repId(ii).eq.120 .and. ! sun state
     &        repId(ii+1).eq.121 .and. ! polar effect
     &        repId(ii+2).eq.122 .and. ! elevation
     &        repId(ii+3).eq.123 .and. ! azimuth
     &        repId(ii+4).eq.124) then ! range
C     ! write sun information
            t2000=repJD(ii)+offset
            call getDate(s2000,t2000,yy1,mm1,dd1,hh1,mi1,sec1)
            write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
            call chop0(rep250(ii),250)
            lens=length(rep250(ii),250,20)
            x50=getValN50(repval(ii),lenx,4)
            y50=getValN50(repval(ii+1),leny,4)
            z50=getValN50(repval(ii+2),lenz,1)
            if (nint(repVal(ii)).eq.1) then
               u50="true"
            else
               u50="false"
            end if
            lenu=len(trim(u50))
            if (nint(repVal(ii+4)).eq.1) then
               v50="day"
            else if (nint(repVal(ii+4)).eq.0) then
               v50="none"
            else if (nint(repVal(ii+4)).eq.-1) then
               v50="night"
            end if
            lenv=len(trim(v50))
            write(s250,120)dat25,x50(1:lenx),y50(1:leny),z50(1:lenz),
     &           u50(1:lenu),v50(1:lenv)
            call chop0(s250,250)
            lens=length(s250,250,30)
            inline=min(imaxline,inline+1)
            write(iline250(inline),'(A)') '   '//s250(1:lens)
            ilenline(inline)=len(trim(iline250(inline)))
         end if
      end do

C     100 - REPORT LOCAL INITIAL MOON STATE
      eventId=100
      nrep=0
      irc=0
      tstopjd=tstartjd+1.0D0
      call astroEvent(
     &     tstartJD,            ! start time (in julianDay)
     &     searchUntil,          ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
      do ii=1,nrep-7
         if (repId(ii).eq.100 .and. ! above horison
     &        repId(ii+1).eq.101 .and. ! polar
     &        repId(ii+2).eq.102 .and. ! phase
     &        repId(ii+3).eq.103 .and. ! clock angle
     &        repId(ii+4).eq.104 .and. ! illumination angle
     &        repId(ii+5).eq.105 .and. ! elevation
     &        repId(ii+6).eq.106 .and. ! azimuth
     &        repId(ii+7).eq.107) then ! range
C     ! write moon shadow information
            x50=getValN50(repval(ii),lenx,4) ! elevation
            y50=getValN50(repval(ii+1),leny,4) ! azimuth
            z50=getValN50(repval(ii+2),lenz,1) ! range
            r50=getValN50(repval(ii+6),lenr,1) ! clock
            t50=getValN50(repval(ii+5),lent,1) ! incidence
            w50=getValN50(repval(ii+7),lenw,1) ! phase
            call chop0(rep250(ii),250)
            lens=length(rep250(ii+3),250,20)
            if (nint(repVal(ii)).eq.1) then
               u50="true"
            else
               u50="false"
            end if
            lenu=len(trim(u50))
            if (nint(repVal(ii+4)).eq.1) then
               v50="day"
            else if (nint(repVal(ii+4)).eq.0) then
               v50="none"
            else if (nint(repVal(ii+4)).eq.-1) then
               v50="night"
            end if
            lenv=len(trim(v50))
            write(s250,100)dat25,
     &           x50(1:lenx),y50(1:leny),z50(1:lenz),
     &           r50(1:lenr),t50(1:lent),
     &           w50(1:lenw),
     &           u50(1:lenu),v50(1:lenv)
            call chop0(s250,250)
            lens=length(s250,250,30)
            inline=min(imaxline,inline+1)
            write(iline250(inline),'(A)') '   '//s250(1:lens)
            ilenline(inline)=len(trim(iline250(inline)))
         end if
      end do
C
C     close time tag
      write(s250,91)
      call chop0(s250,250)
      lens=length(s250,250,30)
      inline=min(imaxline,inline+1)
      write(iline250(inline),'(A)') '  '//s250(1:lens)
      ilenline(inline)=len(trim(iline250(inline)))
C     write events tag if requested
      if (events) then
         write(s250,95)
         call chop0(s250,250)
         lens=length(s250,250,30)
         inline=min(imaxline,inline+1)
         write(iline250(inline),'(A)') '  '//s250(1:lens)
         ilenline(inline)=len(trim(iline250(inline)))
         !
         nline=0
C     990 - DETECT LOCAL SOLAR ECLIPSE
         eventId=990
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
         dat25=""
         a25=""
         b25=""
         c25=""
         d25=""
         lena=0
         lenb=0
         lenc=0
         lend=0
         lene=0
         do ii=1,nrep
            if (repId(ii).eq.990) then ! start
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(a25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lena=length(a25,25,1)
               ! 991=> annular
            else if (repId(ii).eq.992) then ! total start
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(b25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lenb=length(b25,25,1)
            else if (repId(ii).eq.993) then ! maxocc
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(c25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lenc=length(c25,25,1)
               x50=getValn50(repval(ii),lenx,4)
            else if (repId(ii).eq.994) then ! total stop
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(d25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lend=length(d25,25,1)
               ! 995 => annular stop
            else if (repId(ii).eq.996) then ! stop
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(e25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lene=length(e25,25,1)
            end if
         end do
         lens=0
         if (lena.ne.0) then
            if (lena.ne.0) then
               s250=s250(1:lens)//' start="'//a25(1:lena)//'"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            if (lenb.ne.0) then
               s250=s250(1:lens)//' total_start="'//b25(1:lenb)//'"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            if (lenc.ne.0) then
               s250=s250(1:lens)//' max="'//c25(1:lenc)//
     &              '" occ="'//x50(1:lenx)//'%"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            if (lend.ne.0) then
               s250=s250(1:lens)//' total_stop="'//d25(1:lend)//'"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            if (lene.ne.0) then
               s250=s250(1:lens)//' stop="'//e25(1:lene)//'"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            call chop0(s250,250)
            lens=length(s250,250,30)
            nline=min(maxline,nline+1)
            jdline(nline)=t2000
            write(line250(nline),990)s250(1:lens)
            lenline(nline)=len(trim(line250(nline)))
         end if
 990  FORMAT('   <solarEclipse ',A,'/>')
         !
         do ii=1,nline
            indline(ii)=ii
         end do
         call heapsort1(nline,jdline,newnn,nline,indline,1.0D-9,.false.)
         !
         do ii=1,newnn
            inline=min(imaxline,inline+1)
            iline250(inline)=line250(indline(ii))
            ilenline(inline)=lenline(indline(ii))
         end do
         !
         write(s250,96)
         call chop0(s250,250)
         lens=length(s250,250,30)
         inline=min(imaxline,inline+1)
         write(iline250(inline),'(A)') '  '//s250(1:lens)
         ilenline(inline)=len(trim(iline250(inline)))
      end if
C     close location tag
      write(s250,81)
      call chop0(s250,250)
      lens=length(s250,250,30)
      inline=min(imaxline,inline+1)
      write(iline250(inline),'(A)') ' '//s250(1:lens)
      ilenline(inline)=len(trim(iline250(inline)))


      if (events) then
         write(s250,95)
         call chop0(s250,250)
         lens=length(s250,250,30)
         inline=min(imaxline,inline+1)
         write(iline250(inline),'(A)') ' '//s250(1:lens)
         ilenline(inline)=len(trim(iline250(inline)))
         !
         nline=0
         !
         tstartjd=date-offset
C     150 - DETECT SOUTHERN SOLSTICE
         eventId=150
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.150) then ! event
C     ! write southern solstice information
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,150)dat25 ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '  '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 150     FORMAT('<southernSolstice time="',A25,'"/>')

C     160 - DETECT ASCENDING EQUINOX
         eventId=160
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.160) then ! event
C     ! write autumnal solstice information
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,160)dat25 ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '  '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 160     FORMAT('<autumnalEquinox time="',A25,'"/>')

C     170 - DETECT NORTHERN SOLSTICE
         eventId=170
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.170) then ! event
C     ! write northern solstice information
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,170)dat25 ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '  '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 170     FORMAT('<northernSolstice time="',A25,'"/>')

C     180 - DETECT NORTHERN SOLSTICE
         eventId=180
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
            if (repId(ii).eq.180) then ! event
C     ! write vernal equinox information
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               write(s250,180)dat25 ! rep250(ii)(21:lens)
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '  '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 180     FORMAT('<vernalEquinox time="',A25,'"/>')

C     560 - DETECT LUNAR ECLIPSE
         eventId=560
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
         dat25=""
         a25=""
         b25=""
         c25=""
         d25=""
         lena=0
         lenb=0
         lenc=0
         lend=0
         lene=0
         do ii=1,nrep
            if (repId(ii).eq.560) then ! start
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(a25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lena=length(a25,25,1)
            else if (repId(ii).eq.562) then ! total start
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(b25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lenb=length(b25,25,1)
            else if (repId(ii).eq.563) then ! maxocc
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(c25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lenc=length(c25,25,1)
               x50=getValn50(repval(ii),lenx,4)
            else if (repId(ii).eq.564) then ! total start
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(d25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lend=length(d25,25,1)
            else if (repId(ii).eq.566) then ! stop
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               write(e25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
               lene=length(e25,25,1)
            end if
         end do
         lens=0
         if (lena.ne.0) then
            if (lena.ne.0) then
               s250=s250(1:lens)//' start="'//a25(1:lena)//'"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            if (lenb.ne.0) then
               s250=s250(1:lens)//' total_start="'//b25(1:lenb)//'"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            if (lenc.ne.0) then
               s250=s250(1:lens)//' max="'//c25(1:lenc)//
     &              '" occ="'//x50(1:lenx)//'%"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            if (lend.ne.0) then
               s250=s250(1:lens)//' total_stop="'//d25(1:lend)//'"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            if (lene.ne.0) then
               s250=s250(1:lens)//' stop="'//e25(1:lene)//'"'
               call chop0(s250,250)
               lens=length(s250,250,30)
            end if
            call chop0(s250,250)
            lens=length(s250,250,30)
            nline=min(maxline,nline+1)
            jdline(nline)=t2000
            write(line250(nline),560)s250(1:lens)
            lenline(nline)=len(trim(line250(nline)))
         end if
 560  FORMAT('  <lunarEclipse ',A,'/>')


C     210 - DETECT NEW MOON
         eventId=210
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
C     <solarnoon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.210) then ! range
C     ! write new moon information
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C               write(*,*)'REP:',ii,repjd(ii),tstartJD,tstopJD,
C     &              offset ! rep250(ii)(21:lens)
               write(s250,210)dat25
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '  '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 210     FORMAT('<newMoon time="',A25,'"/>')


C     220 - FIRST QUARTER
         eventId=220
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
C     <solarnoon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.220) then ! range
C     ! write first quarter information
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C               write(*,*)'REP:',ii,repjd(ii),tstartJD,tstopJD,
C     &              offset ! rep250(ii)(21:lens)
               write(s250,220)dat25
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '  '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 220     FORMAT('<firstQuarter time="',A25,'"/>')


C     230 - DETECT NEW MOON
         eventId=230
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
C     <solarnoon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.230) then ! range
C     ! write full moon information
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C               write(*,*)'REP:',ii,repjd(ii),tstartJD,tstopJD,
C     &              offset ! rep250(ii)(21:lens)
               write(s250,230)dat25
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '  '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 230     FORMAT('<fullMoon time="',A25,'"/>')



C     240 - DETECT LAST QUARTER
         eventId=240
         nrep=0
         irc=0
         tstopjd=tstartjd
         call astroEvent(
     &        tstartJD,         ! start time (in julianDay)
     &        searchNext,       ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
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
C     <solarnoon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
C     </xsl:template>
         do ii=1,nrep
            if (repId(ii).eq.240) then ! range
C     ! write last quarter information
               t2000=repJD(ii)+offset
               call jd2date(t2000,yy1,mm1,dd1,hh1,mi1,sec1)
               call chop0(rep250(ii),250)
               lens=length(rep250(ii),250,20)
               write(dat25,999)yy1,mm1,dd1,hh1,mi1,int(sec1),off6
C               write(*,*)'REP:',ii,repjd(ii),tstartJD,tstopJD,
C     &              offset ! rep250(ii)(21:lens)
               write(s250,240)dat25
               call chop0(s250,250)
               lens=length(s250,250,30)
               nline=min(maxline,nline+1)
               jdline(nline)=t2000
               write(line250(nline),'(A)') '  '//s250(1:lens)
               lenline(nline)=len(trim(line250(nline)))
            end if
         end do
 240     FORMAT('<lastQuarter time="',A25,'"/>')
         !
         do ii=1,nline
            indline(ii)=ii
         end do
         call heapsort1(nline,jdline,newnn,nline,indline,1.0D-9,.false.)
         !
         do ii=1,newnn
            inline=min(imaxline,inline+1)
            iline250(inline)=line250(indline(ii))
            ilenline(inline)=lenline(indline(ii))
         end do
         !
         write(s250,96)
         call chop0(s250,250)
         lens=length(s250,250,30)
         inline=min(imaxline,inline+1)
         write(iline250(inline),'(A)') ' '//s250(1:lens)
         ilenline(inline)=len(trim(iline250(inline)))
      end if
      !
 80   FORMAT('<location latitude="',A
     &     ,'" longitude="',A,'" height="',A,'">')
 81   FORMAT('</location>')
 90   FORMAT('<time date="',A10,'">')
 91   FORMAT('</time>')
 95   FORMAT('<events>')
 96   FORMAT('</events>')


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
