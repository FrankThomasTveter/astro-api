      subroutine jplephOpen(ijpl1024,crc250,irc)
      implicit none
C
C     Reads JPL ephermeride file and retrieves 
C     + Geocentric, 
C     + Mean of data J2000 (~AISC), 
C     + Cartesian 
C     coordinates for selected celestial objects.
C
      character*1024 ijpl1024      ! name of JPLEPH file
      real day                ! UTC as Julian date since 2000.1.1 0:h
      real pos(6) ! Cartesian Geocentric positions in MDAY(AISC)
      real origo(6) ! Cartesian Geocentric positions in MDAY(AISC)
      character*250 crc250
      integer lencrc
      integer irc               ! Error return code
C
      real x(0:3),y(0:3),z(0:3),N(0:3),out(3)
C
C      real*8 p(3,3)
      real*8 phid,xle,xh        ! geodetic lat,lon,altitude above ellipsoide
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
C      real*8 pos(6,nobj) ! Geocentric Cartesian MJ2000
C      real*8 posCartGeoEF(3,nobj) ! Geocentric Cartesian Earth-Fixed (x-axis pointing towards Greenwich meridian)
C      real*8 posCartLocEF(3,nobj) ! center at location Cartesian Earth-Fixed
C      real*8 posCartTopoEF(3,nobj) ! topocentric Cartesian Earth-Fixed (EAST,NORTH,ZENITH)
C      real*8 posPolarTopoEF(3,nobj) ! topocentric Polar Earth-Fixed (Range, elevation, azimuth) (km,rad)
      integer mode
      parameter (mode=3)        ! only transform pos
C
C     call variables for CONST
C
      CHARACTER*6  NAMS(400)    ! CHARACTER*6 ARRAY OF CONSTANT NAMES
      DOUBLE PRECISION  VALS(400) ! D.P. ARRAY OF VALUES OF CONSTANTS
      integer NVS               !INTEGER NUMBER OF ENTRIES IN 'NAM' AND 'VAL' ARRAYS
      character*1024 jpl1024    ! local name of JPLEPH file
C
      real cosdeg,sindeg
      external cosdeg,sindeg
C     
      real dtlight,dist,lightspeed,p(3,3),pn(3,3), pr(3,3),rr
      parameter (lightspeed=299792.458D0)     ! km/sec
C
      real tai(2)                  ! International Atomic Time
      real tt(2)                   ! Terrestrial Time
      real et(3)                   ! Ephemeris Time
      real utc(2), ut1(2)          ! Universal Time
      INTEGER IY1, IM1, ID1, JS
      REAL FD, DAT
      real RBPN(3,3),RC2T(3,3),RB(3,3),RP(3,3),RBP(3,3)
      real IRBPN(3,3),IRC2T(3,3),IRBP(3,3)
      real TPOS(3), XP,YP,DPSI,DEPS

      LOGICAL FIRST
      DATA FIRST/.TRUE./

      INTEGER LIST(12),II,JJ,PP,key

      double precision RRD(6),ET2Z(2),ET2(2),PV(6,13),pvearth(6)

      double precision CVAL(400),SS(3),AU,emrat
      integer DENUM,NCON,IPT(3,13)
      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT

      LOGICAL KM,BARY
      double precision pvsun(6)
      COMMON/STCOMX/KM,BARY,PVSUN

      double precision PNUT(4),zips(2),
     &     jc2000,obliqeclip,co,so
      data zips/2*0.d0/
C
      logical opened,ready,bdeb
      data opened /.false./,ready/.false./,bdeb/.true./
      character*10 myname
      data myname /'JPLEPH'/
C
      COMMON/JPLOPEN/opened,ready,list,jpl1024,
     &     RRD,ET2Z,ET2,PV,pvearth
C
      real lat,lon,hgt
      real a,b,c,d,f,phi,elong,height,sphi,cphi,teta,ct,st,s1,s2,s3
      real cart(3),polar(3),xyz(3),syz,rqxy,rxy,rq,slon,clon,slat,clat
      integer newcmb
      integer length,lens
      external length
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
C-------------------------------------
C     INITIALISE ROUTINE
C-------------------------------------
C
      jpl1024=ijpl1024
C
      PIH = DATAN2(1.D0,0.D0)
      PI = PIH + PIH
      TWOPI = PI + PI
C  FOR CONVERTING ANGLES FROM RADIANS TO DEGREES AND BACK
      DEG = 18.D1/PI
      RAD = PI/18.D1
C
      call const(NAMS, VALS, SS, NVS, JPL1024)
C     set up target array (request all)
      list(:)=2

      CALL JPLSTATE(zips,list,pv,pnut,jpl1024,crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//
     &        ' < '//myname//' > Error return from JPLSTATE.'
         call chop0(crc250,250)
         return
      end if
      opened=.true.
      return
C
C-------------------------------------
C     CLOSE ROUTINE
C-------------------------------------
C
      entry jplephClose()
      opened=.false.
      ready=.false.
      return
C
C-------------------------------------
C     READ PLANET POSITIONS FROM FILE
C-------------------------------------
C     
      entry JPLephRead(day,crc250,irc)
      if (.not.opened) then
         crc250=myname//' Open before reading.'
         call chop0(crc250,250)
         irc=145
         return
      end if
      
      ! get gregorian calendar from UTC
      call split(day,utc)
!      utc(1)=utc(1)+2451544.5D0 ! convert to true Julian date 
!
      CALL iau_JD2CAL ( utc(1), utc(2), IY1, IM1, ID1, FD, JS )
!      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY1, IM1, ID1, FD, DAT, JS )
!      IF ( JS.LT.0 ) 

!     Atomic time (utc + leap seconds)

      tai(1)=utc(1)
      tai(2)=utc(2)+dat/86400.0D0

!     Terrestrial time

      TT(1) = TAI(1)
      TT(2) = TAI(2) + 32.184D0/86400.0D0

!     ephemeride time (approximately equal to TT on the geoid)
      
      ET(1)=TT(1)
      ET(2)=TT(2)

!     UT1 ~ UTC (approximation to less than a seconds precision)

      ut1(1)=utc(1)
      ut1(2)=utc(2)

c     force km and barycentric output by 'jplstate'
      km=.true.
      bary=.true.
      
C     read ephemerides...
      call jplstate(et,list,pv,pnut,jpl1024,crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//
     &        ' < '//myname//' > Error return from JPLSTATE.'
         call chop0(crc250,250)
         return
      end if
C     Store earth position (barycentric)
      do ii=1,6
         pvearth(ii)=pv(ii,3)-pv(ii,10)/(1.d0+emrat)
      enddo
      ready=.true.
      return
C
C---------------------------------------------------
C     GET EARTH POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephEarthMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (SUN-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pvearth(ii)
      end do
      return
C
C---------------------------------------------------
C     GET SUN GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephSunMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (SUN-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pvsun(ii)-pvearth(ii) ! make sun geocentric
      end do
      return
C
C---------------------------------------------------
C     GET MOON GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephMoonMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (MOON-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pv(ii,10) ! the moon is already geocentric
      end do
      return
C
C---------------------------------------------------
C     GET MERCURY GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephMercuryMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (MERCURY-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pv(ii,1)-pvearth(ii) ! make position geocentric
      end do
      return
C
C---------------------------------------------------
C     GET VENUS GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephVenusMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (VENUS-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pv(ii,2)-pvearth(ii) ! make position geocentric
      end do
      return
C
C---------------------------------------------------
C     GET MARS GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephMarsMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (MARS-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pv(ii,4)-pvearth(ii) ! make position geocentric
      end do
      return
C
C---------------------------------------------------
C     GET JUPITER GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephJupiterMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (JUPITER-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pv(ii,5)-pvearth(ii) ! make position geocentric
      end do
      return
C
C---------------------------------------------------
C     GET SATURN GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephSaturnMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (SATURN-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pv(ii,6)-pvearth(ii) ! make position geocentric
      end do
      return
C
C---------------------------------------------------
C     GET URANUS GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephUranusMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (URANUS-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pv(ii,7)-pvearth(ii) ! make position geocentric
      end do
      return
C
C---------------------------------------------------
C     GET NEPTUNE GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephNeptunMJ(pos,crc250,irc)
      if (.not.ready) then
         crc250=myname//'Routine not ready (NEPTUNE-position).'
         call chop0(crc250,250)
         irc=145
         return
      end if
      do ii=1,6
         pos(ii)=pv(ii,8)-pvearth(ii) ! make position geocentric
      end do
      return
C
C---------------------------------------------------
C     GET PLUTO GEOCENTRIC CARTESIAN POSITION (MEAN SYSTEM OF DATE J2000)
C---------------------------------------------------
C
      entry JPLephPlutoMJ(pos,crc250,irc)
      do ii=1,6
         pos(ii)=pv(ii,9)-pvearth(ii) ! make position geocentric
      end do
      return
C
C---------------------------------------------------
C     CORRECT FOR LIGHT FLIGHT TIME
C---------------------------------------------------
C
      entry JPLephLight(pos,crc250,irc)
      dist=dsqrt(pos(1)*pos(1)
     +     +pos(2)*pos(2)
     +     +pos(3)*pos(3))
      dtlight=dist/lightspeed
      do jj=1,3
         pos(jj)=pos(jj)
     +        -pos(jj+3)*dtlight
      end do
      return
C
C---------------------------------------------------
C     CONVERT FROM MEAN SYSTEM OF J2000 (MJ) TO MEAN OF DATE (MD) (ONLY POSITION)
C---------------------------------------------------
C
      entry JPLephMJtoMDM(day,IRBP,crc250,irc) ! return matrix
!     using "IAU 2000 models"

      call split(day,utc)
!      utc(1)=utc(1)+2451544.5D0 ! convert to true Julian date 
!
      CALL iau_JD2CAL ( utc(1), utc(2), IY1, IM1, ID1, FD, JS )
!      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY1, IM1, ID1, FD, DAT, JS )
!      IF ( JS.LT.0 ) 

!     Atomic time (utc + leap seconds)

      tai(1)=utc(1)
      tai(2)=utc(2)+dat/86400.0D0

!     Terrestrial time

      TT(1) = TAI(1)
      TT(2) = TAI(2) + 32.184D0/86400.0D0

!     get celestial-to-terrestrial matrix (should be true-of-date)...
      xp=0.0D0 ! no polar motion
      yp=0.0D0 ! no polar motion
      CALL iau_BP00 ( TT(1), TT(2), RB, RP, IRBP )
      return

      entry JPLephMJtoMD(day,pos,crc250,irc)
      ! convert MJ-vector -> TD
!     using "IAU 2000 models"

      call split(day,utc)
!      utc(1)=utc(1)+2451544.5D0 ! convert to true Julian date 
!
      CALL iau_JD2CAL ( utc(1), utc(2), IY1, IM1, ID1, FD, JS )
!      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY1, IM1, ID1, FD, DAT, JS )
!      IF ( JS.LT.0 ) 

!     Atomic time (utc + leap seconds)

      tai(1)=utc(1)
      tai(2)=utc(2)+dat/86400.0D0

!     Terrestrial time

      TT(1) = TAI(1)
      TT(2) = TAI(2) + 32.184D0/86400.0D0

!     get celestial-to-terrestrial matrix (should be true-of-date)...
      xp=0.0D0 ! no polar motion
      yp=0.0D0 ! no polar motion
      CALL iau_BP00 ( TT(1), TT(2), RB, RP, RBP )
 
      call iau_RXP(RBP,POS,TPOS)
      pos(1)=tpos(1)
      pos(2)=tpos(2)
      pos(3)=tpos(3)
      return
C---------------------------------------------------
C     CONVERT FROM MEAN SYSTEM OF J2000 (MJ) TO TRUE OF DATE (TD) (ONLY POSITION)
C---------------------------------------------------
C
      entry JPLephMJtoTDM(day,IRBPN,crc250,irc)
!     using "IAU 2000 models"

      call split(day,utc)
!      utc(1)=utc(1)+2451544.5D0 ! convert to true Julian date 
!
      CALL iau_JD2CAL ( utc(1), utc(2), IY1, IM1, ID1, FD, JS )
!      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY1, IM1, ID1, FD, DAT, JS )
!      IF ( JS.LT.0 ) 

!     Atomic time (utc + leap seconds)

      tai(1)=utc(1)
      tai(2)=utc(2)+dat/86400.0D0

!     Terrestrial time

      TT(1) = TAI(1)
      TT(2) = TAI(2) + 32.184D0/86400.0D0

!     get celestial-to-terrestrial matrix (should be true-of-date)...
      xp=0.0D0 ! no polar motion
      yp=0.0D0 ! no polar motion
      CALL iau_PNM00B ( TT(1), TT(2), IRBPN )
      return

      entry JPLephMJtoTD(day,pos,crc250,irc)
!     using "IAU 2000 models"

      call split(day,utc)
!      utc(1)=utc(1)+2451544.5D0 ! convert to true Julian date 
!
      CALL iau_JD2CAL ( utc(1), utc(2), IY1, IM1, ID1, FD, JS )
!      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY1, IM1, ID1, FD, DAT, JS )
!      IF ( JS.LT.0 ) 

!     Atomic time (utc + leap seconds)

      tai(1)=utc(1)
      tai(2)=utc(2)+dat/86400.0D0

!     Terrestrial time

      TT(1) = TAI(1)
      TT(2) = TAI(2) + 32.184D0/86400.0D0

!     get celestial-to-terrestrial matrix (should be true-of-date)...
      xp=0.0D0 ! no polar motion
      yp=0.0D0 ! no polar motion
      CALL iau_PNM00B ( TT(1), TT(2), RBPN )
      call iau_RXP(RBPN,POS,TPOS)
      pos(1)=tpos(1)
      pos(2)=tpos(2)
      pos(3)=tpos(3)
      return
C
C---------------------------------------------------
C     CONVERT FROM MEAN SYSTEM OF J2000 (MJ) TO EARTH FIXED (EF) (ONLY POSITION)
C---------------------------------------------------
C
      entry JPLephMJtoEFM(day,IRC2T,crc250,irc)

!     using "IAU 2000 models"

      call split(day,utc)
!      utc(1)=utc(1)+2451544.5D0 ! convert to true Julian date 
!
      CALL iau_JD2CAL ( utc(1), utc(2), IY1, IM1, ID1, FD, JS )
!      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY1, IM1, ID1, FD, DAT, JS )
!      IF ( JS.LT.0 ) 

!     Atomic time (utc + leap seconds)

      tai(1)=utc(1)
      tai(2)=utc(2)+dat/86400.0D0

!     Terrestrial time

      TT(1) = TAI(1)
      TT(2) = TAI(2) + 32.184D0/86400.0D0

!     get celestial-to-terrestrial matrix (should be true-of-date)...
      xp=0.0D0 ! no polar motion
      yp=0.0D0 ! no polar motion
      CALL iau_NUT00B ( TT(1), TT(2), DPSI, DEPS )
      call iau_C2TPE (TT(1),TT(2),UTC(1),UTC(2),DPSI,DEPS,XP,YP,IRC2T )
      return
      
      entry JPLephMJtoEF(day,pos,crc250,irc)

!     using "IAU 2000 models"

      call split(day,utc)
!      utc(1)=utc(1)+2451544.5D0 ! convert to true Julian date 
!
      CALL iau_JD2CAL ( utc(1), utc(2), IY1, IM1, ID1, FD, JS )
!      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY1, IM1, ID1, FD, DAT, JS )
!      IF ( JS.LT.0 ) 

!     Atomic time (utc + leap seconds)

      tai(1)=utc(1)
      tai(2)=utc(2)+dat/86400.0D0

!     Terrestrial time

      TT(1) = TAI(1)
      TT(2) = TAI(2) + 32.184D0/86400.0D0

!     get celestial-to-terrestrial matrix (should be true-of-date)...
      xp=0.0D0 ! no polar motion
      yp=0.0D0 ! no polar motion
      CALL iau_NUT00B ( TT(1), TT(2), DPSI, DEPS )
      call iau_C2TPE (TT(1),TT(2),UTC(1),UTC(2),DPSI,DEPS,XP,YP,RC2T )

      ! [TRS]  =  RPOM * R_3(GST) * RBPN * [CRS]  =  RC2T * [CRS]

      call iau_RXP(RC2T,POS,TPOS)
C
      pos(1)=tpos(1)
      pos(2)=tpos(2)
      pos(3)=tpos(3)
      return
C
C---------------------------------------------------
C     CONVERT FROM EQUATOR TO ORBIT
C---------------------------------------------------
C
      entry JPLephEQtoORB(day,pos,crc250,irc)
C     Normal plane vector: Z=RxV
      Z(1)=pos(2)*pos(3+3)-pos(3)*pos(3+2)
      Z(2)=pos(3)*pos(3+1)-pos(1)*pos(3+3)
      Z(3)=pos(1)*pos(3+2)-pos(2)*pos(3+1)
C     x-vector: X=(0,0,1)xZ
      X(1)=-Z(2)
      X(2)=Z(1)
      X(3)=0.0D0
C     normalise
      Z(0)=DSQRT(Z(1)*Z(1)+Z(2)*Z(2)+Z(3)*Z(3))
      X(0)=DSQRT(X(1)*X(1)+X(2)*X(2)+X(3)*X(3))
C
      if (Z(0).lt.1.0D-10) THEN ! unable to find orbit plane
         irc=845
         crc250=myname//'Unable to find ORBIT plane.'
      else if (X(0).lt.1.0D-10) THEN ! n and p are parallell
         irc=845
         crc250=myname//'EQUATOR coincides with ORBIT plane.'
      else
         Y(1)=Z(2)*X(3)-Z(3)*X(2)
         Y(2)=Z(3)*X(1)-Z(1)*X(3)
         Y(3)=Z(1)*X(2)-Z(2)*X(1)
         Y(0)=Z(0)*X(0)
         out(1)=(pos(1)*x(1)+pos(2)*x(2)+pos(3)*x(3))/x(0)
         out(2)=(pos(1)*y(1)+pos(2)*y(2)+pos(3)*y(3))/y(0)
         out(3)=(pos(1)*z(1)+pos(2)*z(2)+pos(3)*z(3))/z(0)
         pos(1)=out(1)
         pos(2)=out(2)
         pos(3)=out(3)
      end if
      return
C
C---------------------------------------------------
C     CONVERT FROM EQUATOR TO ECLIPTIC
C---------------------------------------------------
C
      entry JPLephEQtoEC(day,pos,crc250,irc)
      JC2000=DAY/36525.0D0    !  Julian century from the epoch of 2000 as above.
      A = 23.496932D0           ! +- 0.001200 (deg)
      B =-0.86D0                ! +- 0.005 (deg)
      C = 0.01532D0             ! +- 0.0009 rad/Julian century
      D = 4.4D0                 ! +- 0.1 Julian centuries
      obliqEclip = A + B*sin(C*(JC2000 + D))
      co=cosdeg(-obliqEclip)
      so=sindeg(-obliqEclip)
C     make rotation matrix around x-axis
      p(1,1)=1.0D0              ! x-axis is unchanged
      p(1,2)=0.0D0
      p(1,3)=0.0D0
      p(2,1)=0.0D0              ! y-axis is rotated
      p(2,2)=co
      p(2,3)=-so
      p(3,1)=0.0D0              ! z-axis is rotated
      p(3,2)=so
      p(3,3)=co
C     rotate position vector
      call multiply(p,pos)
      return
C
C
C---------------------------------------------------
C     CONVERT FROM EARTH FIXED TO TOPOCENTRIC
C---------------------------------------------------
C
      entry JPLephEFtoTC(lat,lon,hgt,pos,cart,polar,crc250,irc)

C     get observer geodetic position in earth-fixed geocentric 

      A = 6378.155 D0 ! mean radius of equator
      F = 1.D0/298.257 D0 ! flattening coefficient
      phi=lat*RAD           ! GEODETIC LATITUDE
      elong=lon*RAD            ! EAST LONGITUDE OF THE STATION
      height=hgt/1000.0D0        ! ALTITUDE OF THE STATION ABOVE THE SURFACE OF THE REFERENCE ELLIPSOID (KM)
 
      call iau_GD2GCE ( A, F, ELONG, PHI, HEIGHT, XYZ, IRC )

C     get topocentric: cartesian
      
      SPHI = DSIN(PHI)
      CPHI = DCOS(PHI)
      SLON = DSIN(ELONG)
      CLON = DCOS(ELONG)
      TETA = 0.0D0 ! GST
      CT = DCOS(TETA)
      ST = DSIN(TETA)
      S1 = POS(1)*CT + POS(2)*ST - XYZ(1)
      S2 =-POS(1)*ST + POS(2)*CT - XYZ(2)
      CART(1) = -SLON*S1 +CLON*S2
      SYZ = CLON*S1 + SLON*S2
      S3= POS(3) - XYZ(3)
      CART(3) = CPHI*SYZ + SPHI*S3
      CART(2) = -SPHI*SYZ + CPHI*S3
      RQXY = CART(1)**2+CART(2)**2
      RXY = DSQRT(RQXY)

C     get topocentric: range, elevation and azimuth
      POLAR(3) = 0.D0
      IF(RXY.GE.1.D-3)
     &  POLAR(3) = PI + DATAN2(-CART(1),-CART(2))
      RQ = RQXY+CART(3)**2
      POLAR(1) = DSQRT(DMAX1(1.D-6,RQ))
      POLAR(2) = DASIN(CART(3)/POLAR(1))

      return
C
      contains
C+++++++++++++++++++++++++++++++++
C
      SUBROUTINE INTERP(BUF,T,NCF,NCM,NA,IFL,PV)
C
C+++++++++++++++++++++++++++++++++
C
C     THIS SUBROUTINE DIFFERENTIATES AND INTERPOLATES A
C     SET OF CHEBYSHEV COEFFICIENTS TO GIVE POSITION AND VELOCITY
C
C     CALLING SEQUENCE PARAMETERS:
C
C       INPUT:
C
C         BUF   1ST LOCATION OF ARRAY OF D.P. CHEBYSHEV COEFFICIENTS OF POSITION
C
C           T   T(1) IS DP FRACTIONAL TIME IN INTERVAL COVERED BY
C               COEFFICIENTS AT WHICH INTERPOLATION IS WANTED
C               (0 .LE. T(1) .LE. 1).  T(2) IS DP LENGTH OF WHOLE
C               INTERVAL IN INPUT TIME UNITS.
C
C         NCF   # OF COEFFICIENTS PER COMPONENT
C
C         NCM   # OF COMPONENTS PER SET OF COEFFICIENTS
C
C          NA   # OF SETS OF COEFFICIENTS IN FULL ARRAY
C               (I.E., # OF SUB-INTERVALS IN FULL INTERVAL)
C
C          IFL  INTEGER FLAG: =1 FOR POSITIONS ONLY
C                             =2 FOR POS AND VEL
C
C
C       OUTPUT:
C
C         PV   INTERPOLATED QUANTITIES REQUESTED.  DIMENSION
C               EXPECTED IS PV(NCM,IFL), DP.
C
C
      IMPLICIT NONE
C
      integer ncf,ncm,na,ifl,np,nv,l,i,j
      double precision buf(ncf,ncm,*),t(2),pv(ncm,*),pc(18),vc(18)
      double precision dna,VFAC,acdt1,temp,tc,twot,dt1
C
      DATA NP/2/
      DATA NV/3/
      DATA TWOT/0.D0/
      DATA PC(1),PC(2)/1.D0,0.D0/
      DATA VC(2)/1.D0/
C
C       ENTRY POINT. GET CORRECT SUB-INTERVxAL NUMBER FOR THIS SET
C       OF COEFFICIENTS AND THEN GET NORMALIZED CHEBYSHEV TIME
C       WITHIN THAT SUBINTERVAL.
C
      
      DNA=DBLE(NA)
      DT1=DINT(T(1))
      TEMP=DNA*T(1)
      L=IDINT(TEMP-DT1)+1

C         TC IS THE NORMALIZED CHEBYSHEV TIME (-1 .LE. TC .LE. 1)

      TC=2.D0*(DMOD(TEMP,1.D0)+DT1)-1.D0

C       CHECK TO SEE WHETHER CHEBYSHEV TIME HAS CHANGED,
C       AND COMPUTE NEW POLYNOMIAL VALUES IF IT HAS.
C       (THE ELEMENT PC(2) IS THE VALUE OF T1(TC) AND HENCE
C       CONTAINS THE VALUE OF TC ON THE PREVIOUS CALL.)

      IF(TC.NE.PC(2)) THEN
        NP=2
        NV=3
        PC(2)=TC
        TWOT=TC+TC
      ENDIF
C
C       BE SURE THAT AT LEAST 'NCF' POLYNOMIALS HAVE BEEN EVALUATED
C       AND ARE STORED IN THE ARRAY 'PC'.
C
      IF(NP.LT.NCF) THEN
        DO 1 I=NP+1,NCF
        PC(I)=TWOT*PC(I-1)-PC(I-2)
    1   CONTINUE
        NP=NCF
      ENDIF
C
C       INTERPOLATE TO GET POSITION FOR EACH COMPONENT
C
      DO 2 I=1,NCM
      PV(I,1)=0.D0
      DO 3 J=NCF,1,-1
      PV(I,1)=PV(I,1)+PC(J)*BUF(J,I,L)
    3 CONTINUE
    2 CONTINUE
      IF(IFL.LE.1) RETURN
C
C       IF VELOCITY INTERPOLATION IS WANTED, BE SURE ENOUGH
C       DERIVATIVE POLYNOMIALS HAVE BEEN GENERATED AND STORED.
C
      VFAC=(DNA+DNA)/T(2)
      VC(3)=TWOT+TWOT
      IF(NV.LT.NCF) THEN
        DO 4 I=NV+1,NCF
        VC(I)=TWOT*VC(I-1)+PC(I-1)+PC(I-1)-VC(I-2)
    4   CONTINUE
        NV=NCF
      ENDIF
C
C       INTERPOLATE TO GET VELOCITY FOR EACH COMPONENT
C
      DO 5 I=1,NCM
      PV(I,2)=0.D0
      DO 6 J=NCF,2,-1
      PV(I,2)=PV(I,2)+VC(J)*BUF(J,I,L)
    6 CONTINUE
      PV(I,2)=PV(I,2)*VFAC
    5 CONTINUE
C
      RETURN
C
      END subroutine INTERP

C+++++++++++++++++++++++++
C
      SUBROUTINE SPLIT(TT,FR)
C
C+++++++++++++++++++++++++
C
C     THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P. INTEGER
C     AND A D.P. FRACTIONAL PART.
C
C     CALLING SEQUENCE PARAMETERS:
C
C       TT = D.P. INPUT NUMBER
C
C       FR = D.P. 2-WORD OUTPUT ARRAY.
C            FR(1) CONTAINS INTEGER PART
C            FR(2) CONTAINS FRACTIONAL PART
C
C            FOR NEGATIVE INPUT NUMBERS, FR(1) CONTAINS THE NEXT
C            MORE NEGATIVE INTEGER; FR(2) CONTAINS A POSITIVE FRACTION.
C
C       CALLING SEQUENCE DECLARATIONS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION FR(2)

C       MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS

      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)

      IF(TT.GE.0.D0 .OR. FR(2).EQ.0.D0) RETURN

C       MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER

      FR(1)=FR(1)-1.D0
      FR(2)=FR(2)+1.D0

      RETURN

      END subroutine SPLIT


C++++++++++++++++++++++++++++++++
C
      SUBROUTINE JPLSTATE(ET2,LIST,PV,PNUT,JPL1024,CRC250,IRC)
C
C++++++++++++++++++++++++++++++++
C
C THIS SUBROUTINE READS AND INTERPOLATES THE JPL PLANETARY EPHEMERIS FILE
C
C     CALLING SEQUENCE PARAMETERS:
C
C     INPUT:
C
C         ET2   DP 2-WORD JULIAN EPHEMERIS EPOCH AT WHICH INTERPOLATION
C               IS WANTED.  ANY COMBINATION OF ET2(1)+ET2(2) WHICH FALLS
C               WITHIN THE TIME SPAN ON THE FILE IS A PERMISSIBLE EPOCH.
C
C                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
C                   ENTIRE EPOCH IN ET2(1) AND SET ET2(2)=0.
C
C                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET ET2(1) =
C                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
C                   EPOCH AND SET ET2(2) = FRACTIONAL PART OF A DAY
C                   ELAPSED BETWEEN ET2(1) AND EPOCH.
C
C                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
C                   ET2(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
C                   AND ET2(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
C
C        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
C               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
C
C                         LIST(I)=0, NO INTERPOLATION FOR BODY I
C                                =1, POSITION ONLY
C                                =2, POSITION AND VELOCITY
C
C               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
C
C                         I = 1: MERCURY
C                           = 2: VENUS
C                           = 3: EARTH-MOON BARYCENTER
C                           = 4: MARS
C                           = 5: JUPITER
C                           = 6: SATURN
C                           = 7: URANUS
C                           = 8: NEPTUNE
C                           = 9: PLUTO
C                           =10: GEOCENTRIC MOON
C                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
C                           =12: LUNAR LIBRATIONS (IF ON FILE)
C
C
C     OUTPUT:
C
C          PV   DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
C               QUANTITIES.  THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
C               STATE IN THE ARRAY STARTING AT PV(1,I).  (ON ANY GIVEN
C               CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE AFFECTED BY THE
C               FIRST 10 'LIST' ENTRIES (AND BY LIST(12) IF LIBRATIONS ARE
C               ON THE FILE) ARE SET.  THE REST OF THE 'PV' ARRAY
C               IS UNTOUCHED.)  THE ORDER OF COMPONENTS STARTING IN
C               PV(1,I) IS: X,Y,Z,DX,DY,DZ.
C
C               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
C               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
C               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200. 
C
C               THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES 
C               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC, 
C               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
C
C               LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
C               LIST(12) IS 1 OR 2.
C
C         NUT   DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
C               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
C               QUANTITIES IN NUT IS:
C
C                        D PSI  (NUTATION IN LONGITUDE)
C                        D EPSILON (NUTATION IN OBLIQUITY)
C                        D PSI DOT
C                        D EPSILON DOT
C
C           *   STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
C               RANGE OR I/O ERRORS.
C
C
C     COMMON AREA STCOMX:
C
C          KM   LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE OUTPUT
C               STATES. KM = .TRUE., KM AND KM/SEC
C                          = .FALSE., AU AND AU/DAY
C               DEFAULT VALUE = .FALSE.  (KM DETERMINES TIME UNIT
C               FOR NUTATIONS AND LIBRATIONS.  ANGLE UNIT IS ALWAYS RADIANS.)
C
C        BARY   LOGICAL FLAG DEFINING OUTPUT CENTER.
C               ONLY THE 9 PLANETS ARE AFFECTED.
C                        BARY = .TRUE. =\ CENTER IS SOLAR-SYSTEM BARYCENTER
C                             = .FALSE. =\ CENTER IS SUN
C               DEFAULT VALUE = .FALSE.
C
C       PVSUN   DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC POSITION AND
C               VELOCITY OF THE SUN.
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      integer mcore,ncore,lcore,ccore
      parameter (mcore=5)

      double precision ET2(2),PV(6,12),PNUT(4),T(2),PJD(4)
      double precision BUF(1500,mcore)
      character*250 crc250
      integer lencrc
      integer irc

      INTEGER LIST(12),i,j,k,nrl(mcore)

      LOGICAL FIRST
      DATA FIRST/.TRUE./

      CHARACTER*6 CNAM(400),TTL(14,3)
      CHARACTER*1024 JPL1024

      double precision CVAL(400),SS(3),AU,emrat
      integer DENUM,NCON,IPT(3,13),numde

      LOGICAL KM,BARY
      double precision PVSUN(6)
C
      integer irecsz,nrecl,mrecl,ksize,nd,khi,ncoeffs,kmx,nrfile,nr

      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
      COMMON/CHRHDR/CNAM,TTL
      COMMON/STCOMX/KM,BARY,PVSUN
      common/CSTATE/buf,nrl,nrfile,NRECL,ncore,lcore,ccore,ncoeffs
C
      integer length,lens
      external length
C
C       ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
C
      IF(FIRST) THEN
         FIRST=.FALSE.
         ncore=0                ! number of records in core
         lcore=0                ! last record read into core
         ccore=0                ! current record
         
C     ************************************************************************
C     NRECL=1 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN S.P. WORDS
C     NRECL=4 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN BYTES
C     (for UNIX, it is probably 4)
         NRECL=4
C     NRFILE IS THE INTERNAL UNIT NUMBER USED FOR THE EPHEMERIS FILE
         NRFILE=12
C     **  OPEN THE DIRECT-ACCESS FILE AND GET THE POINTERS IN ORDER TO 
C     **  DETERMINE THE SIZE OF THE EPHEMERIS RECORD
         MRECL=NRECL*1000
         lens=length(jpl1024,1024,10)
         OPEN(NRFILE,
     *        FILE=JPL1024(1:lens),
     *        ACCESS='DIRECT',
     *        FORM='UNFORMATTED',
     *        RECL=MRECL,
     *        STATUS='OLD',
     *        IOSTAT=IRC)
         if (irc.ne.0) then
            write(*,*)myname,'Unable to open:',jpl1024(1:lens)
            return
         end if
         READ(NRFILE,REC=1,IOSTAT=IRC)TTL,CNAM,SS,NCON,AU,EMRAT,
     *        ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3)
         if (irc.ne.0) then
            write(*,*)myname,'Unable to read:',jpl1024(1:lens)
            return
         end if
         CLOSE(NRFILE,IOSTAT=IRC)
         if (irc.ne.0) then
            crc250=myname//'Unable to close:'//jpl1024(1:lens)
            return
         end if

C     FIND THE NUMBER OF EPHEMERIS COEFFICIENTS FROM THE POINTERS
         KMX = 0
         KHI = 0
         DO I = 1,13
            IF (IPT(1,I) .GT. KMX) THEN
               KMX = IPT(1,I)
               KHI = I
            ENDIF
         ENDDO
         ND = 3
         IF (KHI .EQ. 12) ND=2
         KSIZE = 2*(IPT(1,KHI)+ND*IPT(2,KHI)*IPT(3,KHI)-1)
         IF(NRECL .EQ. 0) WRITE(*,*)'  ***** Unable to find RECL *****'
         
C     ************************************************************************
C     ************************************************************************
         
         IRECSZ=NRECL*KSIZE
         NCOEFFS=KSIZE/2
         
         OPEN(NRFILE,
     *        FILE=JPL1024(1:lens),
     *        ACCESS='DIRECT',
     *        FORM='UNFORMATTED',
     *        RECL=IRECSZ,
     *        STATUS='OLD')
         
         READ(NRFILE,REC=1)TTL,CNAM,SS,NCON,AU,EMRAT,
     .        ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3)
         
         READ(NRFILE,REC=2)CVAL
         
         NRL(:)=0
         
      ENDIF


C       ********** MAIN ENTRY POINT **********


      IF(ET2(1) .EQ. 0.D0) RETURN

      S=ET2(1)-.5D0
      CALL SPLIT(S,PJD(1))
      CALL SPLIT(ET2(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)+.5D0
      PJD(2)=PJD(2)+PJD(4)
      CALL SPLIT(PJD(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)

C       ERROR RETURN FOR EPOCH OUT OF RANGE

      IF(PJD(1)+PJD(4).LT.SS(1) .OR. PJD(1)+PJD(4).GT.SS(2)) GO TO 98

C       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL

      NR=IDINT((PJD(1)-SS(1))/SS(3))+3
      IF(PJD(1).EQ.SS(2)) NR=NR-1

      tmp1 = DBLE(NR-3)*SS(3) + SS(1)
      tmp2 = PJD(1) - tmp1
      T(1) = (tmp2 + PJD(4))/SS(3)
      
C       READ CORRECT RECORD IF NOT IN CORE

      ccore=0
      do ii=1,ncore
         if (nr.eq.nrl(ii)) then
            ccore=ii
         end if
      end do
      IF(ccore.eq.0) THEN
C        write(*,*)'STATE:',nr
        lcore=lcore+1
        if (lcore.gt.mcore) lcore=1
        READ(NRFILE,REC=NR,ERR=99)(BUF(K,lcore),K=1,NCOEFFS)
        ncore=max(lcore,ncore)
        ccore=lcore
        nrl(lcore)=nr
      ENDIF

      IF(KM) THEN
      T(2)=SS(3)*86400.D0
      AUFAC=1.D0
      ELSE
      T(2)=SS(3)
      AUFAC=1.D0/AU
      ENDIF

C   INTERPOLATE SSBARY SUN

      CALL INTERP(BUF(IPT(1,11),ccore),T,IPT(2,11),3,IPT(3,11),2,PVSUN)


      DO I=1,6
      PVSUN(I)=PVSUN(I)*AUFAC
      ENDDO

C   CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED

      DO 4 I=1,10
      IF(LIST(I).EQ.0) GO TO 4

      CALL INTERP(BUF(IPT(1,I),ccore),T,IPT(2,I),3,IPT(3,I),
     & LIST(I),PV(1,I))

      DO J=1,6
       IF(I.LE.9 .AND. .NOT.BARY) THEN
       PV(J,I)=PV(J,I)*AUFAC-PVSUN(J)
       ELSE
       PV(J,I)=PV(J,I)*AUFAC
       ENDIF
      ENDDO

   4  CONTINUE

C       DO NUTATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0)
     * CALL INTERP(BUF(IPT(1,12),ccore),T,IPT(2,12),2,IPT(3,12),
     * LIST(11),PNUT)

C       GET LIBRATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0)
     * CALL INTERP(BUF(IPT(1,13),ccore),T,IPT(2,13),3,IPT(3,13),
     * LIST(12),PV(1,11))

      RETURN

  98  WRITE(crc250,198)ET2(1)+ET2(2),SS(1),SS(2)
 198  format('< STATE > ***  Requested JED,',f12.2,
     * ' not within ephemeris limits,',2f12.2,'  ***')
      call chop0(crc250,250)
      irc=457
      return
C      stop

   99 WRITE(crc250,'(2F12.2,A80)')ET2,'ERROR RETURN IN STATE'
      call chop0(crc250,250)

      irc=458
      return
C      STOP

      END subroutine JPLSTATE
C+++++++++++++++++++++++++++++
C
      SUBROUTINE CONST(NAM,VAL,SSS,N,JPL1024)
C
C+++++++++++++++++++++++++++++
C
C     THIS ENTRY OBTAINS THE CONSTANTS FROM THE EPHEMERIS FILE
C
C     CALLING SEQEUNCE PARAMETERS (ALL OUTPUT):
C
C       NAM = CHARACTER*6 ARRAY OF CONSTANT NAMES
C
C       VAL = D.P. ARRAY OF VALUES OF CONSTANTS
C
C       SSS = D.P. JD START, JD STOP, STEP OF EPHEMERIS
C
C         N = INTEGER NUMBER OF ENTRIES IN 'NAM' AND 'VAL' ARRAYS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      INTEGER N

      CHARACTER*6 NAM(400)
      DOUBLE PRECISION VAL(400),SSS(3),zips(2)
      character*1024 jpl1024
      DOUBLE PRECISION xx(99)
      data zips/2*0.d0/

      double precision dt1
      INTEGER list(12),i
      logical first
      data first/.true./

      double precision CVAL(400),SS(3),AU,emrat
      integer DENUM,NCON,IPT(3,13)

      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT

      CHARACTER*6 CNAM(400),TTL(14,3)
      COMMON/CHRHDR/CNAM,TTL

C  CALL STATE TO INITIALIZE THE EPHEMERIS AND READ IN THE CONSTANTS

      if(first) then
         call jplstate(zips,list,xx,xx,jpl1024,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//
     &           ' < '//myname//' > Error return from JPLSTATE.'
            call chop0(crc250,250)
            return
         end if
      end if 
      first=.false.

      N=NCON

      DO I=1,3
      SSS(I)=SS(I)
      ENDDO

      DO I=1,N
      NAM(I)=CNAM(I)
      VAL(I)=CVAL(I)
      ENDDO

      RETURN

      END subroutine CONST
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
      end subroutine jplephOpen
