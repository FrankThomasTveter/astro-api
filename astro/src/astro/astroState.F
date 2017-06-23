      subroutine astroState(njd,JD30,lat,lon,hgt,
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250,crc250,irc)
!     
!     The "world coordinate system" is "barycentric MJD2000"  (x->equinox, y->R.A.6 z->north-pole)
!     
      implicit none
      integer njd
      character*30 jd30(njd)
      real lat,lon,hgt
      integer :: maxline
      integer :: nline
      integer :: lenline(maxline)
      character*250 :: line250(maxline)
      character*250 crc250
      integer irc
!     
      character*1 c1
      character*30 b30
      character*6 c6
      integer,external :: length
      integer :: lenc
      real :: jd                ! 	IAU, 0 Jan 2010, 0h TT
      integer, parameter :: nbodies=11
      real :: pole(6,nbodies)   ! R.A., Dec, omega, domega/dt (deg/day) , orbits (index), xmu = G * Mass
      data pole / 
     &     286.15,   63.89,   0.0,   14.18, 0.0, 132712440018.0, ! Sun
     &     281.01,   61.45,   0.0,    6.14, 1.0,        22032.0, ! Mercury
     &     272.76,   67.16,   0.0,   -1.48, 1.0,       324859.0, ! Venus
     &     0.0,   90.00,   0.0,  360.99, 1.0,       398601.3, ! Earth
     &     270.00,   66.54,   0.0,   13.18, 4.0,         4904.9, ! Moon
     &     317.67,   52.88,   0.0,  350.89, 1.0,        42828.0, ! Mars
     &     268.06,   64.50,   0.0,  870.54, 1.0,    126686534.0, ! Jupiter
     &     40.59,   83.54,   0.0,  810.79, 1.0,     37931187.0, ! Saturn
     &     257.31,  -15.18,   0.0, -501.16, 1.0,      5793947.0, ! UranusE
     &     299.40,   42.95,   0.0,  536.31, 1.0,      6836529.0, ! Neptun
     &     312.99,    6.16,   0.0,  -56.36, 1.0,         1001.0/ ! PlutoE
      character*10 nam10(0:nbodies)
      data nam10 /
     &     "",
     &     "sun",
     &     "mercury",
     &     "venus",
     &     "earth",
     &     "moon",
     &     "mars",
     &     "jupiter",
     &     "saturn",
     &     "uranus",
     &     "neptune",
     &     "pluto"/
      real RE,FLAT,FLATSQ,CLIGHT, xflatsq
      COMMON /CGEOME/ RE,FLAT,FLATSQ,CLIGHT
      integer ii,jj,tt,ttx
      real jd2000(njd),rot(3,nbodies), pos(6,nbodies)
      integer yy,mm,dd,hh,mi,ind(njd)
      real sec, loc(3,5), p(3,3), ip(3,3)
      real cd,sd,cr,sr,clat,slat,clon,slon
!     
      real cosdeg,sindeg,asindeg,atan2deg
      external cosdeg,sindeg,asindeg,atan2deg
!     
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      real*8 teta,x(3),dist,elev
      integer mode
!     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
!     
      integer   isun,imercury,ivenus,iearth,imoon,imars
      integer   ijupiter,isaturn,iuranus,ineptun,ipluto
      parameter (isun=1,  imercury=2,  ivenus=3,  iearth=4, imoon=5)
      parameter (imars=6, ijupiter=7, isaturn=8, iuranus=9 )
      parameter (ineptun=10, ipluto=11)
      character*10 myname
      data myname /'astroState'/
      real a,f
      parameter (a=6378.155 D0,f=1.D0/298.257 D0)
!     
!     convert dates to jd2000
!     
      do tt=1,njd
         b30=jd30(tt)
         do ii=1,30
            c1=b30(ii:ii)
            if (c1.ne.'.'.and.
     &           c1.ne.'1'.and.
     &           c1.ne.'2'.and.
     &           c1.ne.'3'.and.
     &           c1.ne.'4'.and.
     &           c1.ne.'5'.and.
     &           c1.ne.'6'.and.
     &           c1.ne.'7'.and.
     &           c1.ne.'8'.and.
     &           c1.ne.'9'.and.
     &           c1.ne.'0') b30(ii:ii)=' '
         end do
         read(b30,*,iostat=irc)yy,mm,dd,hh,mi,sec
         if (irc.ne.0) then
            write(crc250,'(A," Unable to read date from:",A,":",A)')
     &           myname,b30,jd30(tt)
            return
         end if
         CALL DATE2JD(JD2000(tt),YY,MM,DD,HH,MI,SEC) 
         write(c6,'(F6.3)')int(sec*1000.0D0)/1000.0D0
         call chop0(c6,6)
         lenc=length(c6,6,6)
         if (lenc.eq.5) c6='0'//c6(1:5)
         write(jd30(tt),
     &        '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",A6,"Z")')
     &        yy,mm,dd,hh,mi,c6
      end do
!     
!     get planetary angular vectors in MJD2000
!     
      yy=2010
      mm=1
      dd=0
      hh=0
      mi=0
      sec=0.0D0

      CALL DATE2JD(JD,YY,MM,DD,HH,MI,SEC) 
      call JPLephMJtoMDM(JD,P,crc250,irc)
      if (irc.ne.0) then
         lenc=length(crc250,250,100)
         write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &        myname,"JPLephMJtoMDM"
         return
      end if
      call invert(p,ip,irc)
      if (irc.ne.0) then
         lenc=length(crc250,250,100)
         write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &        myname,"invert"
         return
      end if
      do ii=1,nbodies
         cr=cosdeg(pole(1,ii))
         sr=sindeg(pole(1,ii))
         cd=cosdeg(pole(2,ii))
         sd=sindeg(pole(2,ii))
         rot(1,ii)=cd*cr
         rot(2,ii)=cd*sr
         rot(3,ii)=sd
         call multiply(ip,rot(1,ii)) ! -> mean of date J2000
         pole(1,ii)=atan2deg(rot(1,ii),rot(2,ii))
         pole(2,ii)=asindeg(rot(3,ii))
      end do
!     
!     print rotation vectors in MJD2000 ( = world coordinate system)
!     
      nline=0
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')
     &     "Content-type: text/xml;"
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')" "
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')'<?xml version="1.0" '//
     &     'encoding="utf-8"?>'
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'("<solarsystem",A,">")')
     &     ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'//
     &     ' xmlns="http://astro.met.no"'//
     &     ' xsi:schemaLocation='//
     &     '"http://astro.met.no/astro state.xsd"'
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'(1X,7(A))') 
     &     '<location latitude="',ccr(lat),
     &     '" longitude="',ccr(lon),
     &     '" height="',ccr(hgt),
     &     '"/>'
      lenline(nline)=len(trim(line250(nline)))
!     
!     PRINT rotation vectors
!     
      nline=min(maxline,nline+1)
      write(line250(nline),'(1X,A,A,A)') '<initial no="',
     &     cci(nbodies),'" ref="MJD2000">'
      lenline(nline)=len(trim(line250(nline)))
      do jj=1,nbodies
         nline=min(maxline,nline+1)
         write(line250(nline),'(2X,17(A))')
     &        '<body ra="',ccr(pole(1,jj)),
     &        '" dec="',ccr(pole(2,jj)),
     &        '" w="',ccr(pole(3,jj)),
     &        '" dwdt="',ccr(pole(4,jj)),
     &        '" main="',trim(nam10(nint(pole(5,jj)))),
     &        '" xmu="',ccr(pole(6,jj)),
     &        '" id="',cci(jj),
     &        '" name="',trim(nam10(jj)),
     &        '"/>'
         lenline(nline)=len(trim(line250(nline)))
      end do
      nline=min(maxline,nline+1)
      write(line250(nline),'(1X,A)') '</initial>'
      lenline(nline)=len(trim(line250(nline)))
!     
!     sort epochs
!     
      do tt=1,njd
         ind(tt)=tt
      end do
      call mergesort(njd,ind,jd2000)
!     
!     loop over epochs
!     
      nline=min(maxline,nline+1)
      write(line250(nline),'(1X,A,A,A)') 
     &     '<times no="',cci(njd),'">'
      lenline(nline)=len(trim(line250(nline)))
!     
      do ttx=1,njd
         tt=ind(ttx)
         nline=min(maxline,nline+1)
         write(line250(nline),'(2X,7(A))') 
     &        '<time id="',cci(tt),
     &        '" dtg="',trim(jd30(tt)),
     &        '" jd2000="',ccr(jd2000(tt)),
     &        '">'
         lenline(nline)=len(trim(line250(nline)))
!     
!     read data from file for current epoch
!     
         call JPLephRead(jd2000(tt),crc250,irc)
         if (irc.ne.0) then
            lenc=length(crc250,250,100)
            write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &           myname,"JPLephRead"
            return
         end if
!     
!     get transformation from EF to MJD2000
!     
         call JPLephMJtoEFM(JD2000(tt),P,crc250,irc)
         if (irc.ne.0) then
            lenc=length(crc250,250,100)
            write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &           myname,"JPLephMJtoEFM"
            return
         end if
         call invert(p,ip,irc)
         if (irc.ne.0) then
            lenc=length(crc250,250,100)
            write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &           myname,"invert"
            return
         end if
!     
!     geographic coordinates... (no earth flattening)
!     
!     
         clat=cosdeg(lat)
         slat=sindeg(lat)
         clon=cosdeg(lon)
         slon=sindeg(lon)
!     
!     x-unity vector in earth fixed (geocentric) coordinates
!     
         loc(1,1)=1.0D0         ! -slon
         loc(2,1)=0.0D0         ! clon
         loc(3,1)=0.0D0         ! 0.0D0
!     
!     y-unity vector in earth fixed (geocentric) coordinates
!     
         loc(1,2)=0.0D0         ! clat*clon
         loc(2,2)=1.0D0         ! clat*slon
         loc(3,2)=0.0D0         ! slat
!     
!     z-unity vector in earth fixed (geocentric) coordinates
!     
         loc(1,3)=0.0D0         ! -slat*clon
         loc(2,3)=0.0D0         ! -slat*slon
         loc(3,3)=1.0D0         ! clat
!     
!     location vector in earth-fixed geocentric coordinates
!     
!     loc(1,4)=clat*clon*(re+max(10.0D0,hgt))
!     loc(2,4)=clat*slon*(re+max(10.0D0,hgt))
!     loc(3,4)=slat*(re+max(10.0D0,hgt))
!     
C     
         phid=lat*RAD           ! GEODETIC LATITUDE
         xle=lon*RAD            ! EAST LONGITUDE OF THE STATION
         xh=hgt/1000.0D0        ! ALTITUDE OF THE STATION ABOVE THE SURFACE OF THE REFERENCE ELLIPSOID (KM)
C     A = 6378.155 D0 ! mean radius of equator
C     F = 1.D0/298.257 D0 ! flattening coefficient
         call iau_GD2GCE ( A, F, xle, phid, xh, r, IRC )
         if (irc.ne.0) then
            lenc=length(crc250,250,100)
            write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &           myname,"iau_GD2GCE"
            return
         end if
!     
         loc(1,4)=r(1)
         loc(2,4)=r(2)
         loc(3,4)=r(3)
!     
!     get zenith vector in earth-fixed coordinates
!     
         xh=xh+1.0D0            ! zenith direction
         call iau_GD2GCE ( A, F, xle, phid, xh, r, IRC )
         if (irc.ne.0) then
            lenc=length(crc250,250,100)
            write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &           myname,"iau_GD2GCE"
            return
         end if
         loc(1,5)=r(1)-loc(1,4)
         loc(2,5)=r(2)-loc(2,4)
         loc(3,5)=r(3)-loc(3,4)
         xh=dsqrt(loc(1,5)*loc(1,5)+loc(2,5)*
     &        loc(2,5)+loc(3,5)*loc(3,5))
         if (xh.lt.1.0D-9) then
            loc(1,5)=0.0D0
            loc(2,5)=0.0D0
            loc(3,5)=1.0D0
         else
            loc(1,5)=loc(1,5)/xh
            loc(2,5)=loc(2,5)/xh
            loc(3,5)=loc(3,5)/xh
         end if
!     
         do ii=1,5
            call multiply(ip,loc(1,ii)) ! earth-fixed -> MJ2000
         end do

         nline=min(maxline,nline+1)
         write(line250(nline),'(3X,A)') 
     &        '<observer  body="4" ref="MJD2000">'
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(4X,7(A))')
     &        '<i x="',ccr(loc(1,1)),
     &        '" y="', ccr(loc(2,1)),
     &        '" z="', ccr(loc(3,1)),
     &        '"/>' 
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(4X,7(A))')
     &        '<j x="',ccr(loc(1,2)),
     &        '" y="', ccr(loc(2,2)),
     &        '" z="', ccr(loc(3,2)),
     &        '"/>' 
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(4X,7(A))')
     &        '<k x="',ccr(loc(1,3)),
     &        '" y="', ccr(loc(2,3)),
     &        '" z="', ccr(loc(3,3)),
     &        '"/>' 
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(4X,8(A))')
     &        '<location x="',ccr(loc(1,4)),
     &        '" y="', ccr(loc(2,4)),
     &        '" z="', ccr(loc(3,4)),
     &        '" origo="earth"',
     &        '/>'  
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(4X,8(A))')
     &        '<zenith x="',ccr(loc(1,5)),
     &        '" y="', ccr(loc(2,5)),
     &        '" z="', ccr(loc(3,5)),
     &        '"/>' 
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(3X,A)')'</observer>'
         lenline(nline)=len(trim(line250(nline)))
!     
!     get barycentric earth-fixed location vector and coordinate axis in MJD2000 
!     
         call JPLephMJtoMDM(JD2000(tt),P,crc250,irc)
         if (irc.ne.0) then
            lenc=length(crc250,250,100)
            write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &           myname,"JPLephMJtoMDM"
            return
         end if
         call invert(p,ip,irc)
         if (irc.ne.0) then
            lenc=length(crc250,250,100)
            write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &           myname,"invert"
            return
         end if
!     
!     get earth barycentric position MJD2000 in "km"
!     
         call JPLephEarthMJ(pos(1,iearth),crc250,irc)
         if (irc.ne.0) then
            lenc=length(crc250,250,100)
            write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &           myname,"JPLephEarthMJ"
            return
         end if
!     
!     convert body positions from geocentric to barycentric MJD2000
!     
         do jj=1,nbodies
            if (jj.eq.isun) then
               call JPLephSunMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephSunMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.imercury) then
               call JPLephMercuryMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephMercuryMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.ivenus) then
               call JPLephVenusMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephVenusMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.iearth) then ! earth
            else if (jj.eq.imoon) then
               call JPLephMoonMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephMoonMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.imars) then
               call JPLephMarsMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephMarsMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.ijupiter) then
               call JPLephJupiterMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephJupiterMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.isaturn) then
               call JPLephSaturnMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephSaturnMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.iuranus) then
               call JPLephUranusMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephUranusMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.ineptun) then
               call JPLephNeptunMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephNeptunMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            else if (jj.eq.ipluto) then
               call JPLephPlutoMJ(pos(1,jj),crc250,irc)
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephPlutoMJ"
                  return
               end if
               call JPLephLight(pos(1,jj),crc250,irc) ! correct for light flight time...
               if (irc.ne.0) then
                  lenc=length(crc250,250,100)
                  write(crc250,FMT=100)crc250(1:min(200,lenc)),
     &                 myname,"JPLephLight"
                  return
               end if
               do ii=1,6
                  pos(ii,jj)=pos(ii,jj) + pos(ii,iearth)
               end do
            end if
         end do
!     
!     print result to screen
!     
!     CALL JD2DATE(JD2000(tt),YY,MM,DD,HH,MI,SEC) 
         nline=min(maxline,nline+1)
         write(line250(nline),'(3X,A,A,A)') '<state no="',cci(nbodies),
     &        '" ref="MJD2000">'
         lenline(nline)=len(trim(line250(nline)))
         do jj=1,nbodies
            nline=min(maxline,nline+1)
            write(line250(nline),'(4X,17(A))')
     &           '<body x="',ccr(pos(1,jj)),
     &           '" y="',ccr(pos(2,jj)),
     &           '" z="',ccr(pos(3,jj)),
     &           '" vx="',ccr(pos(4,jj)),
     &           '" vy="',ccr(pos(5,jj)),
     &           '" vz="',ccr(pos(6,jj)),
     &           '" id="',cci(jj),
     &           '" name="',trim(nam10(jj)),
     &           '"/>'
            lenline(nline)=len(trim(line250(nline)))
         end do
         nline=min(maxline,nline+1)
         write(line250(nline),'(3X,A)') '</state>'
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(2X,A)') '</time>'
         lenline(nline)=len(trim(line250(nline)))
      end do
      nline=min(maxline,nline+1)
      write(line250(nline),'(1X,A)') '</times>'
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'("</solarsystem>",A)')" "
      lenline(nline)=len(trim(line250(nline)))

      return

 100  FORMAT(A,X,A,' Error return from ',A)

      contains

      function ccr(r) result(res)
      character(:),allocatable :: res
      real,intent(in) :: r
      integer ::length,lent,ii,pos
      external :: length
      character(50) :: tmp
      write(tmp,'(F27.12)') r
      call chop0(tmp,50)
      lent=length(tmp,50,10)
      pos=1
      do while (pos.le.lent)
         if (tmp(pos:pos).eq.".") then
            ii=lent
            do while (ii.gt.pos+1)
               if (tmp(ii:ii).eq.'0') then
                  lent=lent-1
                  ii=ii-1
               else
                  ii=pos
               end if
            end do
            pos=lent+1
         else
            pos=pos+1
         endif
      end do
      res = tmp(1:lent)
      end function ccr

      function cci(i) result(res)
      character(:),allocatable :: res
      integer,intent(in) :: i
      integer ::length,lent
      external :: length
      character(50) :: tmp
      write(tmp,'(I0)') i
      call chop0(tmp,50)
      lent=length(tmp,50,10)
      res = tmp(1:lent)
      end function cci

      subroutine multiply(p,ri)
C     Vector multiplication: ro = p*ri
      implicit none
      real p(3,3),ri(3),ro(3)
      integer ii,jj
      DO ii=1,3
         ro(II)=0.0D0
         DO JJ=1,3
            ro(II)=ro(II)+P(II,JJ)*ri(JJ)
         END DO
      END DO
      DO JJ=1,3
         ri(jj)=ro(jj)
      end do
      return
      end subroutine multiply
      end subroutine astroState
!     
      subroutine invert(a,t,irc)
      real a(3,3), t(3,3),d
      D = A(1,1)*A(2,2)*A(3,3)-A(1,1)*A(2,3)*A(3,2)-
     &     A(1,2)*A(2,1)*A(3,3)+A(1,2)*A(2,3)*A(3,1)+
     &     A(1,3)*A(2,1)*A(3,2)-A(1,3)*A(2,2)*A(3,1)
      if (abs(d).gt.1.0D-10) then
         T(1,1) = (A(2,2)*A(3,3)-A(2,3)*A(3,2))/D
         T(1,2) = -(A(1,2)*A(3,3)-A(1,3)*A(3,2))/D
         T(1,3) = (A(1,2)*A(2,3)-A(1,3)*A(2,2))/D
         T(2,1) = -(A(2,1)*A(3,3)-A(2,3)*A(3,1))/D
         T(2,2) = (A(1,1)*A(3,3)-A(1,3)*A(3,1))/D
         T(2,3) = -(A(1,1)*A(2,3)-A(1,3)*A(2,1))/D
         T(3,1) = (A(2,1)*A(3,2)-A(2,2)*A(3,1))/D
         T(3,2) = -(A(1,1)*A(3,2)-A(1,2)*A(3,1))/D
         T(3,3) = (A(1,1)*A(2,2)-A(1,2)*A(2,1))/D
         irc=0
      else
         T(:,:)=0
         T(1,1)=1.0D0
         T(2,2)=1.0D0
         T(3,3)=1.0D0
         write(*,*) 'INVERT Unable to invert: ',d,a
      end if
      return
      end subroutine invert
      subroutine mergesort(n,ind,key)
      
      implicit none
      integer n
      integer ind(n)
      integer, allocatable :: tmp(:)
      real key(n)
      
      allocate(tmp(n))
      call mergesort1(ind, key, tmp, n, 1, n)
      deallocate (tmp)
      
      end subroutine mergesort

      recursive subroutine mergesort1(ind, key, tmp, n, l, r)
      
      implicit none
      
      integer n
      integer ind(n)
      real key(n)
      integer tmp(n)
      integer l, r
      
      integer i, j, k, m
      
      if (r > l) then
         m = (r+l)/2
         call mergesort1(ind, key, tmp, n, l, m)
         call mergesort1(ind, key, tmp, n, m+1, r)
         do i = l, m            !m, l, -1
            tmp(i) = ind(i)
         end do
         do j = m+1, r
            tmp(r+m+1-j) = ind(j)
         end do
         i = l
         j = r
         do k = l, r
            if (key(tmp(i)) < key(tmp(j))) then
               ind(k) = tmp(i)
               i = i+1
            else
               ind(k) =tmp(j)
               j = j-1
            end if
         end do
      end if
      
      end subroutine mergesort1

