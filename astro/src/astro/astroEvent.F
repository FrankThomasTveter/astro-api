C     
C     ASTRONOMICAL EVENTS - ASTROEVENT
C     
C     subroutine "ASTROEVENT" searches for and reports requested 
C     astronomical events based on the JPL-ephemerides (DE405). 
C     The available astronomical events are listed in the 
C     "astronomical event identification table" below. 
C     
C     The "ASTROEVENTs" subroutine finds the event time and reports
C     relevant data. The reported relevant data is also listed in 
C     the "astronomical event identification table" below.
C     
C     "ASTROEVENT" assumes "JPLephOpen(jpl1024,crc250,irc)", has already
C     been called (opens JPL ephemeride file for reading).
C     "JPLephClose(jpl1024,crc250,irc)" should be called after calculations
C     have been completed (closes the JPL ephemeride file).
C     The JPL epemeride interface subroutines are found in "jpleph.F".
C     
      subroutine ASTROEVENT(
     &     tstartJD,          ! start time (in julianDay)
     &     searchCode,          ! search code; -1:previous, +1:next, 0: both, +2:until tendJD
     &     tendJD,            ! report all events end time (in julianDay)
     &     eventId,             ! requested event id (SEE TABLE BELOW)
     &     neventVal,           ! number of event input data (SEE TABLE BELOW)
     &     eventVal,            ! event input data (SEE TABLE BELOW)
     &     maxrep,              ! maximum number of output reports
     &     nrep,                ! number of output reports
     &     repJD,             ! time of output report (in julianDay)
     &     repId,               ! output report identification (SEE TABLE BELOW)
     &     repVal,              ! output report value (SEE TABLE BELOW)
     &     rep250,              ! output report string (redundant description)
     &     secdec,              ! number of second decimals used in output report string 
     &     crc250,              ! error description
     &     irc)                 ! error return code (0=ok)
C     
C     ASTRONOMICAL EVENT IDENTIFICATION TABLE
C     
C     EVENTID  = 100  'REPORT LOCAL INITIAL MOON STATE'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 100 -> if (repval >= 1) "moon is above horison" (repval <=-1) "moon is below"
C     o  repId = 101 -> if (repval >= 1) "lunar polar day" (repval =0) "no lunar polar effect" (repval<=-1) "lunar polar night"
C     o  repId = 102 -> moon phase (0<=repval<25 = "waxing crescent", 25<=repval<50="waxing gibbous",50<=repval<75="waning gibbous",75<=repval<100="waning crescent")
C     EVENTID  = 105 : 'REPORT LOCAL VISIBLE MOON IN PERIOD'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 105 -> repVal = hours visible moon in period
C     o  repId = 106 -> Moon rise.
C     o  repId = 107 -> Moon set.
C     
C     EVENTID  = 110 : 'REPORT LOCAL TC EF MOON POSITION AT TIME INCREMENT'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     i  eventVal(4) = time increment (days)
C     o  repId = 110 -> repval = moon elevation (deg)
C     o  repId = 111 -> repval = moon azimuth (deg)
C     o  repId = 112 -> repval = moon range (km)
C     
C     EVENTID  = 120 : 'REPORT LOCAL INITIAL SUN STATE'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 120 -> if (repval >= 1) "sun is above horison" (repval <=-1) "sun is below"
C     o  repId = 121 -> if (repval >= 1) "polar day" (repval =0) "no polar effect" (repval<=-1) "polar night"
C     EVENTID  = 125 : 'REPORT LOCAL VISIBLE SUN IN PERIOD'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 125 -> repVal = hours visible sun in period
C     o  repId = 126 -> Sun rise.
C     o  repId = 127 -> Sun set.
C     
C     EVENTID  = 130 : 'REPORT LOCAL TC EF SUN POSITION AT TIME INCREMENT'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     i  eventVal(4) = time increment (days)
C     o  repId = 130 -> repval = sun elevation (deg)
C     o  repId = 131 -> repval = sun azimuth (deg)
C     o  repId = 132 -> repval = sun range (km)
      
C     EVENTID  = 150 : 'DETECT SOUTHERN SOLSTICE'
C     o  repId = 150 -> event found
C     EVENTID  = 160 : 'DETECT ASCENDING SOLAR EQUINOX'
C     o  repId = 160 -> event found
C     EVENTID  = 170 : 'DETECT NORTHERN SOLSTICE'
C     o  repId = 170 -> event found
C     EVENTID  = 180 : 'DETECT DESCENDING SOLAR EQUINOX'
C     o  repId = 180 -> event found
C     EVENTID  = 190 : 'DETECT EARTH IN PERIHELION'
C     o  repId = 190 -> repval = sun range (km)
C     EVENTID  = 200 : 'DETECT EARTH IN APHELION'
C     o  repId = 200 -> repval = sun range (km)
      
C     EVENTID  = 210 : 'DETECT NEW MOON (PHASE=0/100)'
C     o  repId = 210 -> event found
C     EVENTID  = 220 : 'DETECT FIRST QUARTER MOON (PHASE=25)'
C     o  repId = 220 -> event found
C     EVENTID  = 230 : 'DETECT FULL MOON (PHASE=50)'
C     o  repId = 230 -> event found
C     EVENTID  = 240 : 'DETECT LAST QUARTER MOON (PHASE=75)'
C     o  repId = 240 -> event found
C     EVENTID  = 250 : 'DETECT MOON PHASE (0 TO 100)'
C     i  eventVal(1) = target moon phase
C     o  repId = 250 -> event found
C     EVENTID  = 260 : 'DETECT MOON ILLUMINATION MINIMUM'
C     o  repId = 260 -> event found
C     EVENTID  = 270 : 'DETECT MOON ILLUMINATION MAXIMUM'
C     o  repId = 270 -> event found
C     EVENTID  = 280 : 'DETECT MOON ILLUMINATION (0 TO 100)'
C     i  eventVal(1) = target moon illumination
C     o  repId = 280 -> event found
      
C     EVENTID  = 290 : 'DETECT SOUTHERN LUNASTICE'
C     o  repId = 290 -> event found
C     EVENTID  = 292 : 'DETECT ASCENDING LUNAR EQUINOX'
C     o  repId = 292 -> event found
C     EVENTID  = 294 : 'DETECT NORTHERN LUNASTICE'
C     o  repId = 294 -> event found
C     EVENTID  = 296 : 'DETECT DESCENDING LUNAR EQUINOX'
C     o  repId = 296 -> event found
      
C     EVENTID  = 300 : 'DETECT MERCURY INFERIOR CONJUNCTION'
C     o  repId = 300 -> event found
C     EVENTID  = 310 : 'DETECT MERCURY SUPERIOR CONJUNCTION'
C     o  repId = 310 -> event found
C     EVENTID  = 320 : 'DETECT MERCURY GREATEST WESTERN ELONGATION'
C     o  repId = 320 -> event found
C     EVENTID  = 330 : 'DETECT MERCURY GREATEST EASTERN ELONGATION'
C     o  repId = 330 -> event found
      
C     EVENTID  = 340 : 'DETECT VENUS INFERIOR CONJUNCTION'
C     o  repId = 340 -> event found
C     EVENTID  = 350 : 'DETECT VENUS GREATEST WESTERN ELONGATION'
C     o  repId = 350 -> event found
C     EVENTID  = 360 : 'DETECT VENUS SUPERIOR CONJUNCTION'
C     o  repId = 360 -> event found
C     EVENTID  = 370 : 'DETECT VENUS GREATEST EASTERN ELONGATION'
C     o  repId = 370 -> event found
      
C     EVENTID  = 380 : 'DETECT MARS CONJUNCTION'
C     o  repId = 380 -> event found
C     EVENTID  = 390 : 'DETECT MARS WESTERN QUADRATURE'
C     o  repId = 390 -> event found
C     EVENTID  = 400 : 'DETECT MARS OPPOSITION'
C     o  repId = 400 -> event found
C     EVENTID  = 410 : 'DETECT MARS EASTERN QUADRATURE'
C     o  repId = 410 -> event found
      
C     EVENTID  = 420 : 'DETECT JUPITER CONJUNCTION'
C     o  repId = 420 -> event found
C     EVENTID  = 430 : 'DETECT JUPITER WESTERN QUADRATURE'
C     o  repId = 430 -> event found
C     EVENTID  = 440 : 'DETECT JUPITER OPPOSITION'
C     o  repId = 440 -> event found
C     EVENTID  = 450 : 'DETECT JUPITER EASTERN QUADRATURE'
C     o  repId = 450 -> event found
      
C     EVENTID  = 460 : 'DETECT SATURN CONJUNCTION'
C     o  repId = 460 -> event found
C     EVENTID  = 470 : 'DETECT SATURN WESTERN QUADRATURE'
C     o  repId = 470 -> event found
C     EVENTID  = 480 : 'DETECT SATURN OPPOSITION'
C     o  repId = 480 -> event found
C     EVENTID  = 490 : 'DETECT SATURN EASTERN QUADRATURE'
C     o  repId = 490 -> event found
      
C     EVENTID  = 500 : 'DETECT MERCURY TRANSIT (ANYWHERE ON EARTH)'
C     o  repId = 500 -> transit starts
C     o  repId = 501 -> transit ends
C     EVENTID  = 520 : 'DETECT VENUS TRANSIT (ANYWHERE ON EARTH)'
C     o  repId = 520 -> transit starts
C     o  repId = 521 -> transit ends
      
C     EVENTID  = 550 : 'DETECT LUNAR ECLIPSE (MINOCC MAXOCC)'
C     i  eventVal(1) = minimum occultation (0 to 100)
C     i  eventVal(2) = maximum occultation (0 to 100)
C     o  repId = 550 -> penumbra contact starts (P1)
C     o  repId = 551 -> umbra contact starts (U1)
C     o  repId = 552 -> total eclipse starts (U2)
C     o  repId = 553 -> repval = maximum occultation
C     o  repId = 554 -> total eclipse stops (U3)
C     o  repId = 555 -> umbra contact stops (U4)
C     o  repId = 556 -> penumbra contact stops (P2)
C     EVENTID  = 560 : 'DETECT LUNAR ECLIPSE -LUNECL[0]'
C     o  repId = 560 -> penumbra contact starts (P1)
C     o  repId = 561 -> umbra contact starts (U1)
C     o  repId = 562 -> total eclipse starts (U2)
C     o  repId = 563 -> repval = maximum occultation
C     o  repId = 564 -> total eclipse stops (U3)
C     o  repId = 565 -> umbra contact stops (U4)
C     o  repId = 566 -> penumbra contact stops (P2)
      
C     EVENTID  = 600 : 'DETECT LOCAL DIURNAL SUN RISE'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 600 -> event found
C     EVENTID  = 610 : 'DETECT LOCAL DIURNAL SUN SET'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 610 -> event found
C     EVENTID  = 620 : 'DETECT LOCAL DIURNAL MAXIMUM SOLAR ELEVATION'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 620 -> repval = maximum solar elevation (deg)
C     EVENTID  = 630 : 'DETECT LOCAL DIURNAL MINIMUM SOLAR ELEVATION'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 630 -> repval = minimum solar elevation (deg)
C     EVENTID  = 640 : 'DETECT LOCAL DIURNAL CIVIL TWILIGHT START'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 640 -> event found
C     EVENTID  = 650 : 'DETECT LOCAL DIURNAL CIVIL TWILIGHT STOP'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 650 -> event found
C     EVENTID  = 660 : 'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT START'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 660 -> event found
C     EVENTID  = 670 : 'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT STOP'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 670 -> event found
C     EVENTID  = 680 : 'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT START'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 680 -> event found
C     EVENTID  = 690 : 'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT STOP'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 690 -> event found
C     EVENTID  = 700 : 'DETECT LOCAL DIURNAL NIGHT START'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 700 -> event found
C     EVENTID  = 710 : 'DETECT LOCAL DIURNAL NIGHT STOP'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 710 -> event found
C     EVENTID  = 750 : 'DETECT LOCAL DIURNAL SUN AZIMUTH (0=NORTH, 90=EAST)'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     i  eventVal(4) = sun azimuth (deg)
C     o  repId = 750 -> event found
C     EVENTID  = 760 : 'DETECT LOCAL DIURNAL APPARENT SOLAR TIME'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     i  eventVal(4) = apparent solar time (0 to 24)
C     o  repId = 760 -> event found
C     EVENTID  = 770 : 'DETECT LOCAL DIURNAL APPARENT LUNAR TIME'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     i  eventVal(4) = apparent lunar time (0 to 24)
C     o  repId = 770 -> event found

C     EVENTID  = 800 : 'DETECT LOCAL DIURNAL MOON RISE'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 800 -> event found
C     EVENTID  = 810 : 'DETECT LOCAL DIURNAL MOON SET'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 810 -> event found
C     EVENTID  = 820 : 'DETECT LOCAL DIURNAL MAXIMUM MOON ELEVATION'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 820 -> repVal = maximum moon elevation
C     EVENTID  = 830 : 'DETECT LOCAL DIURNAL MINIMUM MOON ELEVATION'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 830 -> repVal = minimum moon elevation
C     EVENTID  = 840 : 'DETECT LOCAL DIURNAL MOON AZIMUTH (0=NORTH, 90=EAST)'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     i  eventVal(4) = moon azimuth (deg)
C     o  repId = 840 -> event found
      
C     EVENTID  = 900 : 'DETECT LOCAL POLAR SUN DAY START'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 900 -> event found
C     o  repId = 901 -> previous sun rise
C     EVENTID  = 910 : 'DETECT LOCAL POLAR SUN DAY STOP'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 910 -> event found
C     o  repId = 911 -> next sun set
C     EVENTID  = 920 : 'DETECT LOCAL POLAR SUN NIGHT START'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 920 -> event found
C     o  repId = 921 -> previous sun set
C     EVENTID  = 930 : 'DETECT LOCAL POLAR SUN NIGHT STOP'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 930 -> event found
C     o  repId = 931 -> next sun rise
C     
C     EVENTID  = 940 : 'DETECT LOCAL POLAR LUNAR DAY START'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 940 -> event found
C     o  repId = 941 -> previous moon rise
C     EVENTID  = 950 : 'DETECT LOCAL POLAR LUNAR DAY STOP'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 950 -> event found
C     o  repId = 951 -> next moon set
C     EVENTID  = 960 : 'DETECT LOCAL POLAR LUNAR NIGHT START'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 960 -> event found
C     o  repId = 961 -> previous moon set
C     EVENTID  = 970 : 'DETECT LOCAL POLAR LUNAR NIGHT STOP'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 970 -> event found
C     o  repId = 971 -> next moon rise
C     
C     EVENTID  = 980 : 'DETECT LOCAL SOLAR ECLIPSE (MINOCC MAXOCC)'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     i  eventVal(4) = minimum occultation (0 to 100)
C     i  eventVal(5) = maximum occultation (0 to 100)
C     o  repId = 980 -> partial solar eclipse starts
C     o  repId = 981 -> total solar eclipse starts
C     o  repId = 982 -> repVal = maximum occultation
C     o  repId = 983 -> total solar eclipse stops
C     o  repId = 984 -> partial solar eclipse stops
C     EVENTID  = 990 : 'DETECT LOCAL SOLAR ECLIPSE'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     o  repId = 990 -> partial solar eclipse starts
C     o  repId = 991 -> total solar eclipse starts
C     o  repId = 992 -> repVal = maximum occultation
C     o  repId = 993 -> total solar eclipse stops
C     o  repId = 994 -> partial solar eclipse stops
C     
C     EVENTID  = 1000 : 'REPORT LOCAL TC EF SOLAR SYSTEM POSITIONS AT TIME INCREMENT'
C     i  eventVal(1) = latitude of observer (deg)
C     i  eventVal(2) = longtitude of observer (deg)
C     i  eventVal(3) = height of observer (deg)
C     i  eventVal(4) = time increment (days)
C     o  repId = 1000 -> repval = sun elevation (deg)
C     o  repId = 1001 -> repval = sun azimuth (deg)
C     o  repId = 1002 -> repval = sun range (km)
C     o  repId = 1010 -> repval = mercury elevation (deg)
C     o  repId = 1011 -> repval = mercury azimuth (deg)
C     o  repId = 1012 -> repval = mercury range (km)
C     o  repId = 1020 -> repval = venus elevation (deg)
C     o  repId = 1021 -> repval = venus azimuth (deg)
C     o  repId = 1022 -> repval = venus range (km)
C     o  repId = 1030 -> repval = moon elevation (deg)
C     o  repId = 1031 -> repval = moon azimuth (deg)
C     o  repId = 1032 -> repval = moon range (km)
C     o  repId = 1040 -> repval = mars elevation (deg)
C     o  repId = 1041 -> repval = mars azimuth (deg)
C     o  repId = 1042 -> repval = mars range (km)
C     o  repId = 1050 -> repval = jupiter elevation (deg)
C     o  repId = 1051 -> repval = jupiter azimuth (deg)
C     o  repId = 1052 -> repval = jupiter range (km)
C     o  repId = 1060 -> repval = saturn elevation (deg)
C     o  repId = 1061 -> repval = saturn azimuth (deg)
C     o  repId = 1062 -> repval = saturn range (km)
C     o  repId = 1070 -> repval = uranus elevation (deg)
C     o  repId = 1071 -> repval = uranus azimuth (deg)
C     o  repId = 1072 -> repval = uranus range (km)
C     o  repId = 1080 -> repval = neptun elevation (deg)
C     o  repId = 1081 -> repval = neptun azimuth (deg)
C     o  repId = 1082 -> repval = neptun range (km)
C     o  repId = 1090 -> repval = pluto elevation (deg)
C     o  repId = 1091 -> repval = pluto azimuth (deg)
C     o  repId = 1092 -> repval = pluto range (km)
C     
      implicit none
C     
      real tstartJD           ! start time (in julianDay)
      integer searchCode        ! search code; -1:previous, +1:next, 0: both, +2:until tendJD
      real tendJD             ! report all events end time (in julianDay)
      integer eventId           ! requested event id (SEE TABLE BELOW)
      integer neventVal         ! number of event input data (SEE TABLE ABOVE)
      real eventVal(max(1,neventVal))  ! event input data (SEE TABLE ABOVE)
      integer maxrep            ! maximum number of output reports
      integer nrep              ! number of output reports
      real  repJD(maxrep)     ! time of output report (in julianDay)
      integer repId(maxrep)     ! output report identification (SEE TABLE ABOVE)
      real repVal(maxrep)       ! output report value (SEE TABLE ABOVE)
      character*250 rep250(maxrep) ! output report string (redundant)
      integer secdec            ! number of second decimals in output report string 
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C     
      character*100 event100
      character*250 buff250
      integer ii,jj
      logical lnext             ! search for next event
      logical lprev             ! search for previous event
      logical lall              ! report all events
      character*10 id10
C
      real dt
C
      real dtmoon,dtss,dtsun,illum,lat,lon,hgt,tJD
      real target,solartime,lunartime,sunazi,moonazi,moccmax,moccmin
      real phase,soccmax,soccmin,refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
      logical lmoccmax,lmoccmin,lsoccmax,lsoccmin
      integer inrep,jnrep,lend,lene,length
      external length
C
      character*12 myname
      data myname /'ASTROEVENT'/
      logical bdeb
      bdeb=.false.
C
      if (irc.eq.0) crc250=''
      call chop0(crc250,250)
C
C     determine search direction
C
      lnext=.false.
      lprev=.false.
      lall=.false.
      dt=0.0D0
      if (searchCode.eq.-1) then ! search for previous event
         lprev=.true.
         tendJD=tstartJD
      elseif (searchCode.eq.0) then ! search for previous and next event
         lprev=.true.
         lnext=.true.
         tendJD=tstartJD
      elseif (searchCode.eq.-2) then ! search for previous and next event with offset
         lprev=.true.
         lnext=.true.
         tendJD=tstartJD
         dt=1.0D0/86400.0D0
      elseif (searchCode.eq.+1) then ! search for next event
         lnext=.true.
         tendJD=tstartJD
      else                      ! report all events until tendJD
         lall=.true.
      end if
C
       secdec=max(0,min(3,secdec))
C
      if (bdeb) then
         write(*,*)myname,"Entering.",irc
         write(*,*)myname,"tstartJD:",tstartJD ! start time (in julianDay)
         write(*,*)myname,"searchCode:",searchCode ! search for next event
         write(*,*)myname,"lnext:",lnext ! search for next event
         write(*,*)myname,"lprev:",lprev ! search for previous event
         write(*,*)myname,"lall:",lall ! report all events (no search)
         write(*,*)myname,"tendJD:",tendJD ! report all events end time (in julianDay)
         write(*,*)myname,"eventId:",eventId ! requested event id (SEE TABLE BELOW)
         write(*,*)myname,"neventVal:",neventVal ! number of event input data (SEE TABLE BELOW)
         write(*,*)myname,"eventVal:",(eventVal(ii),ii=1,neventVal) ! event input data (SEE TABLE BELOW)
         write(*,*)myname,"maxrep:",maxrep ! maximum number of output reports
         write(*,*)myname,"nrep:",nrep ! number of output reports
         write(*,*)myname,"repJD:",(repJD(ii),ii=1,nrep) ! time of output report (in julianDay)
         write(*,*)myname,"repId:",(repId(ii),ii=1,nrep) ! output report identification (SEE TABLE BELOW)
         write(*,*)myname,"repVal:",(repVal(ii),ii=1,nrep) ! output report value (SEE TABLE BELOW)
         write(*,*)myname,"rep250:",(rep250(ii),ii=1,nrep) ! output report string (redundant description)
         write(*,*)myname,"secdec:",secdec ! number of second decimals used in output report string 
      end if
C
      IF (eventId.eq.100) THEN  !  'REPORT INITIAL MOON STATE'
         event100='LOCAL MOON STATE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         call reportMoonInit(
     &        lat,lon,hgt,
     &        tstartJD,secdec,
     &        eventId,event100,maxrep,nrep,repJD,
     &        repid,repval,rep250,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           '> Error return from reportMoonInit.'
            call chop0(crc250,250)
            return
         end if
      ELSE IF (eventId.eq.105) THEN !  'REPORT VISIBLE MOON IN PERIOD'
         event100='VISIBLE MOON'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then
            call reportMoonVisible(
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,maxrep,nrep,repJD,
     &           repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              '> Error return from reportMoonVisible.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tendJD=tstartJD-1.0D0
               call reportMoonVisible(
     &              lat,lon,hgt,
     &              tstartJD-dt,tendJD-dt,secdec,
     &              eventId,event100,maxrep,nrep,repJD,
     &              repid,repval,rep250,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 '> Error return from reportMoonVisible.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tendJD=tstartJD+1.0D0
               call reportMoonVisible(
     &              lat,lon,hgt,
     &              tstartJD+dt,tendJD+dt,secdec,
     &              eventId,event100,maxrep,nrep,repJD,
     &              repid,repval,rep250,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 '> Error return from reportMoonVisible.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.110) THEN !  'REPORT MOON POSITION AT TIME INCREMENT'
         event100='LOCAL MOON POSITION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3.and.neventVal.lt.4) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 or 4 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (neventVal.eq.4) then
            dtmoon=eventVal(4)
         else
            dtmoon=0.0D0
            tendJD=tstartJD
         end if
C     if (lall) then ! if search only process initial time
         call reportMoonPos(
     &        lat,lon,hgt,
     &        tstartJD,tendJD,dtmoon,secdec,
     &        eventId,event100,
     &        maxrep,nrep,repJD,repid,repval,rep250,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           '> Error return from reportMoonPos.'
            call chop0(crc250,250)
            return
         end if
C     end if
      ELSE IF (eventId.eq.120) THEN !  'REPORT INITIAL SUN STATE'
         event100='LOCAL SUN STATE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         call reportSunInit(
     &        lat,lon,hgt,
     &        tstartJD,secdec,
     &        eventId,event100,
     &        maxrep,nrep,repJD,repid,repval,rep250,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from reportSunInit.'
            call chop0(crc250,250)
            return
         end if
      ELSE IF (eventId.eq.125) THEN !  'REPORT VISIBLE SUN IN PERIOD'
         event100='VISIBLE SUN'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then
            call reportSunVisible(
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,maxrep,nrep,repJD,
     &           repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              '> Error return from reportSunVisible.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tendJD=tstartJD-1.0D0
               call reportSunVisible(
     &              lat,lon,hgt,
     &              tstartJD-dt,tendJD-dt,secdec,
     &              eventId,event100,maxrep,nrep,repJD,
     &              repid,repval,rep250,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 '> Error return from reportSunVisible.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tendJD=tstartJD+1.0D0
               call reportSunVisible(
     &              lat,lon,hgt,
     &              tstartJD+dt,tendJD+dt,secdec,
     &              eventId,event100,maxrep,nrep,repJD,
     &              repid,repval,rep250,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 '> Error return from reportSunVisible.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.130) THEN !  'REPORT SUN POSITION AT TIME INCREMENT'
         event100='LOCAL SUN POSITION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3.and.neventVal.lt.4) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 or 4 event values, got",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (neventVal.eq.4) then
            dtsun=eventVal(4)
         else
            dtsun=0.0D0
            tendJD=tstartJD
         end if
C     if (lall) then ! report for initial time only if search
         call reportSunPos(
     &        lat,lon,hgt,
     &        tstartJD,tendJD,dtsun,secdec,
     &        eventId,event100,
     &        maxrep,nrep,repJD,repid,repval,rep250,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from sunPos.'
            call chop0(crc250,250)
            return
         end if
C     end if
      ELSE IF (eventId.eq.150) THEN !  'DETECT SOUTHERN SOLSTICE[0]'
         event100='SOUTHERN SOLSTICE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=-90.0D0         ! target value
         if (lall) then         ! report all
            call reportSunRA(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from SOUTHERN.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunRA(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SOUTHERN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunRA(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SOUTHERN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.160) THEN !  'DETECT ASCENDING SOLAR EQUINOX -VERNAL[0]'
         event100= 'ASCENDING SOLAR EQUINOX'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=0.0D0           ! target value
         if (lall) then         ! report all
            call reportSunRA(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from ASCENDING.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunRA(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from ASCENDING.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunRA(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from ASCENDING.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.170) THEN !  'DETECT NORTHERN SOLSTICE -SUMMER[0]'
         event100='NORTHERN SOLSTICE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=90.0D0          ! target value
         if (lall) then         ! report all
            call reportSunRA(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from NORTHERN.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunRA(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NORTHERN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunRA(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NORTHERN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.180) THEN !  'DETECT DESCENDING SOLAR EQUINOX -AUTUMNAL[0]'
         event100= 'DESCENDING SOLAR EQUINOX'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=180.0D0         ! sun celestial longtitude
         if (lall) then         ! report all
            call reportSunRA(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from DESCENDING.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then     ! previous event
               tJD=searchSunRA(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from DESCENDING.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then     ! next event
               tJD=searchSunRA(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from DESCENDING.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.190) THEN !  'DETECT EARTH IN PERIHELION -PERIHELION[0]'
         event100= 'EARTH IN PERIHELION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMinSunEarthDist(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from PERIHELION.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMinSunEarthDist(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMinSunEarthDist(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.195) THEN !  'DETECT EARTH IN APHELION -APHELION[0]'
         event100=   'EARTH IN APHELION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMaxSunEarthDist(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from APHELION.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMaxSunEarthDist(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from APHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMaxSunEarthDist(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from APHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.200) THEN !  'DETECT MOON IN PERIGEE -PERIGEE[0]'
         event100= 'MOON IN PERIGEE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMinMoonEarthDist(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from PERIHELION.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMinMoonEarthDist(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMinMoonEarthDist(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.205) THEN !  'DETECT EARTH IN APOGEE [0]'
         event100=   'MOON IN APOGEE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMaxMoonEarthDist(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from APHELION.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMaxMoonEarthDist(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from APHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMaxMoonEarthDist(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from APHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.210) THEN !  'DETECT NEW MOON (PHASE=0/100) -MOONNEW[0]'
         event100='NEW MOON (PHASE=0/100)'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=0.0D0           ! target moon phase
         if (lall) then         ! report all
            call reportMoonPhase(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MOON PHASE.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonPhase(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON PHASE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonPhase(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON PHASE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.220) THEN !  'DETECT FIRST QUARTER MOON (PHASE=25) -MOONFIRST[0]'
         event100='FIRST QUARTER MOON (PHASE=25)'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=25.0D0          ! target moon phase
         if (lall) then         ! report all
            call reportMoonPhase(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MOON PHASE.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonPhase(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              11,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON PHASE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonPhase(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON PHASE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.230) THEN !  'DETECT FULL MOON (PHASE=50) -MOONFULL[0]'
         event100='FULL MOON (PHASE=50)'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=50.0D0          ! target moon phase
         if (lall) then         ! report all
            call reportMoonPhase(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MOON PHASE.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonPhase(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON PHASE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonPhase(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON PHASE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.240) THEN !  'DETECT LAST QUARTER MOON (PHASE=75) -MOONLAST[0]'
         event100='LAST QUARTER MOON (PHASE=75)'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=75.0D0          ! target moon phase
         if (lall) then         ! report all
            call reportMoonPhase(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MOON PHASE.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonPhase(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON PHASE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonPhase(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON PHASE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.250) THEN !  'DETECT MOON PHASE (0 TO 100) -MOONPHASE[*]'
         if (neventVal.lt.1) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 1 event value, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         phase=eventVal(1)
         target=max(0.0D0,min(100.0D0,phase)) ! target phase
         write(event100,'("MOON PHASE (PHASE=",I3,")")') 
     &        nint(target)
         if (lall) then         ! report all
            call reportMoonPhase(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MOON'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonPhase(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonPhase(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.260) THEN !  'DETECT MOON ILLUMINATION MINIMUM -MINMOONILLUM[*]'
         event100='MOON ILLUMINATION MINIMUM'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMinMoonIllum(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MINMOONILLUM.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMinMoonIllum(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MINMOONILLUM.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMinMoonIllum(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MINMOONILLUM.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.270) THEN !  'DETECT MOON ILLUMINATION MAXIMUM -MAXMOONILLUM[*]'
         event100='MOON ILLUMINATION MAXIMUM'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMaxMoonIllum(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MAXMOONILLUM.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMaxMoonIllum(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MAXMOONILLUM.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMaxMoonIllum(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MAXMOONILLUM.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.280) THEN !  'DETECT MOON ILLUMINATION (0 TO 100) -MOONILLUM[*]'
         if (neventVal.lt.1) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 1 event value, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         illum=eventVal(1)
         target=max(0.0D0,min(100.0D0,illum)) ! target illumination
         write(event100,'("MOON ILLUMINATION (",F10.2,")")') 
     &        target
         if (lall) then         ! report all
            call reportMoonIllum(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MOON'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonIllum(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonIllum(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOON'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
C     
C     Mercury 4° 6' 0"      4.1 deg/day           87.97 Earth Days   0.24 Earth Years 
C     Venus   1° 36' 0"     1.6 deg/day          224.70 Earth days 
C     The Earth 0° 59' 8"   0.98556 deg/day      365.25 days
C     Mars    0° 31' 0"     0.527 deg/day        686.93 Earth days 365.26 Earth Days 
C     Jupiter 0° 4'59"      0.0830556 deg/day   4331.89 days   11.86 Earth years 
C     Saturn  0° 2' 1"      0.033611 deg/day   10760.34 days   29.46 Earth years 
C     Uranus  0° 0' 42"     0.0116667 deg/day  30589.90 days   83.75 Earth years 
C     Neptune 0° 0' 24"     0.0066667 deg/day  59799.14 days  163.72 Earth years 
C     Pluto   0° 0' 14"     0.0038889 deg/day  90553.40 days  247.92 Earth years 
C     The Moon (around the Earth) 13° 10'35"  13.176389 deg/day 27.3216 days
C     
      ELSE IF (eventId.eq.290) THEN !  'DETECT LUNAR SOUTHERN SOLSTICE [0]'
         event100='SOUTHERN LUNASTICE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=-90.0D0         ! target value
         if (lall) then         ! report all
            call reportMoonRA(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from SOUTHERN.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonRA(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SOUTHERN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonRA(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SOUTHERN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.292) THEN !  'DETECT ASCENDING LUNAR EQUINOX'
         event100= 'ASCENDING LUNAR EQUINOX'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=0.0D0           ! target value
         if (lall) then         ! report all
            call reportMoonRA(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from ASCENDING.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonRA(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from ASCENDING.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonRA(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from ASCENDING.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.294) THEN !  'DETECT NORTHERN LUNASTICE [0]'
         event100='NORTHERN LUNASTICE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=90.0D0          ! target value
         if (lall) then         ! report all
            call reportMoonRA(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from NORTHERN.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonRA(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NORTHERN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonRA(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NORTHERN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.296) THEN !  'DETECT DESCENDING LUNAR EQUINOX [0]'
         event100= 'DESCENDING LUNAR EQUINOX'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=180.0D0         ! moon celestial longtitude
         if (lall) then         ! report all
            call reportMoonRA(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from DESCENDING.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then     ! previous event
               tJD=searchMoonRA(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from DESCENDING.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then     ! next event
               tJD=searchMoonRA(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from DESCENDING.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.300) THEN ! 'DETECT MERCURY INFERIOR CONJUNCTION -MERINFCON[0]'
         event100='MERCURY INFERIOR CONJUNCTION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=0.0D0
!     period = 360.0/( 360.0/89.97 - 360.0/365.25 )) = 120 days
         if (lall) then         ! report all
            call reportMercRelLon(target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Merc RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMercRelLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MERC LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMercRelLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MERC LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.310) THEN !  'DETECT MERCURY SUPERIOR CONJUNCTION -MERSUPCON[0]'
         event100='MERCURY SUPERIOR CONJUNCTION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=180.0D0
!     period = 360.0/( 360.0/89.97 - 360.0/365.25 )) = 120 days
         if (lall) then         ! report all
            call reportMercRelLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MERC LON.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMercRelLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MERC LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMercRelLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MERC LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.320) THEN !  'DETECT MERCURY GREATEST WESTERN ELONGATION -MERWESTEL[0]'
         event100='MERCURY GREATEST WESTERN ELONGATION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMaxMercElong(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from PERIHELION.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMaxMercElong(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMaxMercElong(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.330) THEN !  'DETECT MERCURY GREATEST EASTERN ELONGATION -MEREASTEL[0]'
         event100='MERCURY GREATEST EASTERN ELONGATION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMinMercElong(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from PERIHELION.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMinMercElong(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMinMercElong(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.340) THEN !  'DETECT VENUS INFERIOR CONJUNCTION -VENINFCON[0]'
         event100='VENUS INFERIOR CONJUNCTION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=0.0D0
!     period = 360.0/( 360.0/89.97 - 360.0/365.25 )) = 585 days
         if (lall) then         ! report all
            call reportVenusRelLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchVenusRelLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from VENUS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchVenusRelLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from VENUS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.350) THEN !  'DETECT VENUS GREATEST WESTERN ELONGATION -VENWESTEL[0]'
         event100='VENUS GREATEST WESTERN ELONGATION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMaxVenusElong(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from PERIHELION.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMaxVenusElong(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMaxVenusElong(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.360) THEN !  'DETECT VENUS SUPERIOR CONJUNCTION -VENSUPCON[0]'
         event100='VENUS SUPERIOR CONJUNCTION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=180.0D0
         if (lall) then         ! report all
            call reportVenusRelLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchVenusRelLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from VENUS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchVenusRelLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from VENUS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.370) THEN !  'DETECT VENUS GREATEST EASTERN ELONGATION -VENEASTEL[0]'
         event100='VENUS GREATEST EASTERN ELONGATION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMinVenusElong(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from PERIHELION.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMinVenusElong(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMinVenusElong(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PERIHELION.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.380) THEN !  'DETECT MARS CONJUNCTION -MARCON[0]'
         event100='MARS CONJUNCTION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=0.0D0
!     period = 360.0/( 360.0/89.97 - 360.0/365.25 )) = 780 days
         if (lall) then         ! report all
            call reportMarsLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMarsLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MARS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMarsLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MARS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.390) THEN !  'DETECT MARS WESTERN QUADRATURE -MARWEST[0]'
         event100='MARS WESTERN QUADRATURE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=270.0D0
         if (lall) then         ! report all
            call reportMarsLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMarsLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MARS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMarsLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MARS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.400) THEN !  'DETECT MARS OPPOSITION -MAROPP[0]'
         event100='MARS OPPOSITION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=180.0D0
         if (lall) then         ! report all
            call reportMarsLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMarsLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MARS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMarsLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MARS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.410) THEN !  'DETECT MARS EASTERN QUADRATURE -MAREAST[0]'
         event100='MARS EASTERN QUADRATURE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=90.0D0
         if (lall) then         ! report all
            call reportMarsLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMarsLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MARS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMarsLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MARS LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.420) THEN !  'DETECT JUPITER CONJUNCTION -JUPCON[0]'
         event100='JUPITER CONJUNCTION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=0.0D0
!     period = 360.0/( 360.0/89.97 - 360.0/365.25 )) = 398 days
         if (lall) then         ! report all
            call reportJupiterLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchJupiterLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JUPITER LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchJupiterLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JUPITER LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.430) THEN !  'DETECT JUPITER WESTERN QUADRATURE -JUPWEST[0]'
         event100='JUPITER WESTERN QUADRATURE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=270.0D0
         if (lall) then         ! report all
            call reportJupiterLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchJupiterLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JUPITER LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchJupiterLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JUPITER LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.440) THEN !  'DETECT JUPITER OPPOSITION -JUPOPP[0]'
         event100='JUPITER OPPOSITION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=180.0D0
         if (lall) then         ! report all
            call reportJupiterLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchJupiterLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JUPITER LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchJupiterLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JUPITER LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.450) THEN !  'DETECT JUPITER EASTERN QUADRATURE -JUPEAST[0]'
         event100='JUPITER EASTERN QUADRATURE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=90.0D0
         if (lall) then         ! report all
            call reportJupiterLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchJupiterLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JUPITER LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchJupiterLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JUPITER LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.460) THEN !  'DETECT SATURN CONJUNCTION -SATCON[0]'
         event100='SATURN CONJUNCTION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=0.0D0
!     period = 360.0/( 360.0/89.97 - 360.0/365.25 )) = 380 days
         if (lall) then         ! report all
            call reportSaturnLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSaturnLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SATURN LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSaturnLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SATURN LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.470) THEN !  'DETECT SATURN WESTERN QUADRATURE -SATWEST[0]'
         event100='SATURN WESTERN QUADRATURE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=270.0D0
         if (lall) then         ! report all
            call reportSaturnLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSaturnLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SATURN LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSaturnLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SATURN LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.480) THEN !  'DETECT SATURN OPPOSITION -SATOPP[0]'
         event100='SATURN OPPOSITION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=180.0D0
         if (lall) then         ! report all
            call reportSaturnLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSaturnLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SATURN LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSaturnLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SATURN LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.490) THEN !  'DETECT SATURN EASTERN QUADRATURE -SATEAST[0]'
         event100='SATURN EASTERN QUADRATURE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         Target=90.0D0
         if (lall) then         ! report all
            call reportSaturnLon(Target,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus RelLon.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSaturnLon(Target,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SATURN LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSaturnLon(Target,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SATURN LON.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.500) THEN           !  'DETECT MERCURY TRANSIT (ANYWHERE ON EARTH) -MERTRA[0]'
         event100='MERCURY TRANSIT'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportMercTransit(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus MERC PEN.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMercTransit(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MERC PEN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMercTransit(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next transit
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MERC PEN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.520) THEN !  'DETECT VENUS TRANSIT (ANYWHERE ON EARTH) -VENTRA[0]'
         event100='VENUS TRANSIT'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         if (lall) then         ! report all
            call reportVenusTransit(
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Venus VENUS PEN.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchVenusTransit(
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from VENUS PEN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchVenusTransit(
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from VENUS PEN.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.550) THEN !  'DETECT LUNAR ECLIPSE (MIN OCC...
         if (neventVal.lt.2) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 2 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lmoccmin=.true.
         lmoccmax=.true.
         moccmin=eventVal(1)
         moccmax=eventVal(2)
         event100=""
         lend=0
         if (lmoccMin) then
            write(event100(lend+1:100),'(" MINOCC=",F5.1,"%")')
     &           moccmin
            call chop0(event100,100)
            lend=length(event100,100,10)
         end if
         if (lmoccmax) then
            write(event100(lend+1:100),'(" MAXOCC=",F5.1,"%")')
     &           moccmax
            call chop0(event100,100)
            lend=length(event100,100,10)
         end if
         if (lend.ne.0) then
            event100="LUNAR ECLIPSE ("//event100(1:lend)//")"
            if (bdeb)write(*,*)myname,event100
         else
            event100="LUNAR ECLIPSE"
            if (bdeb)write(*,*)myname,event100
         end if
         call chop0(event100,100)
         if (lall) then         ! report all
            call reportUmbra(
     &           tstartJD,tendJD,secdec,
     &           lmoccmax,moccmax,lmoccmin,moccmin,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from UMBRA.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchUmbra(
     &              tstartJD-dt,secdec,
     &              lmoccmax,moccmax,
     &              lmoccmin,moccmin,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from UMBRA.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchUmbra(
     &              tstartJD+dt,secdec,
     &              lmoccmax,moccmax,
     &              lmoccmin,moccmin,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from UMBRA.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.560) THEN !  'DETECT MOON IN UMBRA -UMBRA[0]'
         event100='LUNAR ECLIPSE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.0) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 0 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lmoccmin=.false.
         lmoccmax=.false.
         if (lall) then         ! report all
            call reportUmbra(
     &           tstartJD,tendJD,secdec,
     &           lmoccmax,moccmax,lmoccmin,moccmin,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from UMBRA.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchUmbra(
     &              tstartJD-dt,secdec,
     &              lmoccmax,moccmax,lmoccmin,moccmin,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from UMBRA.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchUmbra(
     &              tstartJD+dt,secdec,
     &              lmoccmax,moccmax,lmoccmin,moccmin,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from UMBRA.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.600) THEN  !  'DETECT LOCAL DIURNAL SUN RISE -SUNRISE[0]%2&3&$'
         event100='LOCAL DIURNAL SUN RISE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         target=-refraction
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunUpperElevation(target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from SUNRISE.'
               call chop0(crc250,250)
               return
            end if
         else 
            if (lprev) then     ! previous
               tJD=searchSunUpperElevation(target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SUNRISE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then     ! next
               tJD=searchSunUpperElevation(target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SUNRISE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.610) THEN !  'DETECT LOCAL DIURNAL SUN SET -SUNSET[0]%2&3&$'
         event100='LOCAL DIURNAL SUN SET'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         Target= -refraction    ! target elevation
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunUpperElevation(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! only look for decreasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from SUNSET.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunUpperElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SUNSET.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunUpperElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SUNSET.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-2.0D-10
         end do
      else IF (eventId.eq.620) THEN  !  'DETECT LOCAL DIURNAL MAXIMUM SOLAR ELEVATION -SUNMAXEL[0]%2&3&$'
         event100='LOCAL DIURNAL MAXIMUM SOLAR ELEVATION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then         ! report all
            call reportMaxSunElevation(
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MAXSUNEL.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMaxSunElevation(
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MAXSUNEL.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMaxSunElevation(
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MAXSUNEL.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.630) THEN !  'DETECT LOCAL DIURNAL MINIMUM SOLAR ELEVATION -SUNMINEL[0]%2&3&$'
         event100='LOCAL DIURNAL MINIMUM SOLAR ELEVATION'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then         ! report all
            call reportMinSunElevation(
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MINSUNEL.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMinSunElevation(
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MINSUNEL.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMinSunElevation(
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MINSUNEL.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.640) THEN !  'DETECT LOCAL DIURNAL CIVIL TWILIGHT START -CIVSTART[0]%2&3&$'
         event100='LOCAL DIURNAL CIVIL TWILIGHT START'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         target=-refraction
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunUpperElevation(target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! search for previous event         ! only look for decreasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from CIVSTART.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then     ! previous
               tJD=searchSunUpperElevation(target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from CIVSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then     ! next
               tJD=searchSunUpperElevation(target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from CIVSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.650) THEN !  'DETECT LOCAL DIURNAL CIVIL TWILIGHT STOP -CIVSTOP[0]%2&3&$'
         event100='LOCAL DIURNAL CIVIL TWILIGHT STOP'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         Target= -refraction    ! target elevation
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunUpperElevation(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from CIVSTOP.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunUpperElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from CIVSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunUpperElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from CIVSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-2.0D-10
         end do
      ELSE IF (eventId.eq.660) THEN !  'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT START -NAUSTART[0]%2&3&$'
         event100='LOCAL DIURNAL NAUTICAL TWILIGHT START'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         Target= - 6.0D0        ! target elevation
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunElevation(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! only look for decreasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from NAUSTART.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NAUSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NAUSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)+2.0D-10
         end do
      ELSE IF (eventId.eq.670) THEN !  'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT STOP -NAUSTOP[0]%2&3&$'
         event100='LOCAL DIURNAL NAUTICAL TWILIGHT STOP'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         Target= - 6.0D0        ! target elevation
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunElevation(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from NAUSTOP.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NAUSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NAUSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-2.0D-10
         end do
      ELSE IF (eventId.eq.680) THEN !  'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT START -ASTSTART[0]%2&3&$'
         event100='LOCAL DIURNAL ASTRONOMICAL TWILIGHT START'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         Target= - 12.0D0       ! target elevation
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunElevation(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! only look for decreasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from ASTSTART.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from ASTSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from ASTSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)+2.0D-10
         end do
      ELSE IF (eventId.eq.690) THEN !  'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT STOP -ASTSTOP[0]%2&3&$'
         event100='LOCAL DIURNAL ASTRONOMICAL TWILIGHT STOP'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         Target= - 12.0D0       ! target elevation
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunElevation(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from ASTSTOP.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from ASTSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from ASTSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-2.0D-10
         end do
      ELSE IF (eventId.eq.700) THEN  !  'DETECT LOCAL DIURNAL NIGHT START -NIGHTSTART[0]%2&3&$'
         event100='LOCAL DIURNAL NIGHT START'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         Target= - 18.0D0       ! target elevation
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunElevation(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! only look for decreasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from NIGHTSTART.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NIGHTSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! only look for decreasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NIGHTSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)+2.0D-10
         end do
      ELSE IF (eventId.eq.710) THEN !  'DETECT LOCAL DIURNAL NIGHT STOP -NIGHTSTOP[0]%2&3&$'
         event100='LOCAL DIURNAL NIGHT STOP'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         Target= -18.0D0        ! target elevation
         inrep=nrep+1
         if (lall) then         ! report all
            call reportSunElevation(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from NIGHTSTOP.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NIGHTSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunElevation(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! only look for increasing elevation
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NIGHTSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-2.0D-10
         end do
      ELSE IF (eventId.eq.750) THEN !  'DETECT LOCAL DIURNAL SUN AZIMUTH (0=NORTH, 90=EAST) -SUNAZI[*]%2&3&$'
         if (neventVal.lt.4) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 4 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         sunazi=eventVal(4)
         event100=""
         lend=0
         write(event100(lend+1:100),'(F5.1)')
     &        sunazi
         call chop0(event100,100)
         lend=length(event100,100,10)
         event100="LOCAL DIURNAL SUN AZIMUTH ("//
     &        event100(1:lend)//")"
         if (bdeb)write(*,*)myname,event100
         call chop0(event100,100)
         if (lall) then         ! report all
            call reportSunAzimuth(sunazi,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from SunAzi.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunAzimuth(sunazi,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SunAzi.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunAzimuth(sunazi,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SunAzi.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.760) THEN !  'DETECT LOCAL DIURNAL APPARENT SOLAR TIME -SOLARTIME[*]%2&3&$'
         if (neventVal.lt.4) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 4 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         solartime=eventVal(4)
         event100=""
         lend=0
         write(event100(lend+1:100),'(F5.1)')
     &        solartime
         call chop0(event100,100)
         lend=length(event100,100,10)
         event100="LOCAL DIURNAL APPARENT SOLAR TIME ("//
     &        event100(1:lend)//")"
         if (bdeb)write(*,*)myname,event100
         call chop0(event100,100)
         if (lall) then         ! report all
            call reportSolarTime(solartime,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Solartime.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSolarTime(solartime,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from Solartime.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSolarTime(solartime,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from Solartime.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.770) THEN !  'DETECT LOCAL DIURNAL APPARENT LUNAR TIME -LUNARTIME[*]%2&3&$'
         if (neventVal.lt.4) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 4 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         lunartime=eventVal(4)
         event100=""
         lend=0
         write(event100(lend+1:100),'(F5.1)')
     &        lunartime
         call chop0(event100,100)
         lend=length(event100,100,10)
         event100="LOCAL DIURNAL APPARENT LUNAR TIME ("//
     &        event100(1:lend)//")"
         if (bdeb)write(*,*)myname,event100
         call chop0(event100,100)
         if (lall) then         ! report all
            call reportLunarTime(lunartime,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Lunartime.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchLunarTime(lunartime,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from Lunartime.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchLunarTime(lunartime,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from Lunartime.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.800) THEN !  'DETECT LOCAL DIURNAL MOON RISE -MOONRISE[0]%2&3&$'
         event100='LOCAL DIURNAL MOON RISE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then         ! report all
            call reportMoonRise(lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MOONRISE.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then     ! previous
               tJD=searchMoonRise(
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOONRISE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then     ! next
               tJD=searchMoonRise(
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              1,          ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOONRISE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.810) THEN !  'DETECT LOCAL DIURNAL MOON SET -MOONSET[0]%2&3&$'
         event100='LOCAL DIURNAL MOON SET'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then         ! report all
            call reportMoonSet(lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MOONSET.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then     ! previous
               tJD=searchMoonSet(
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOONSET.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then     ! next
               tJD=searchMoonSet(
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              1,          ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MOONSET.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      else IF (eventId.eq.820) THEN  !  'DETECT LOCAL DIURNAL MAXIMUM MOON ELEVATION  -MOONMAXEL[0]%2&3&$'
         event100='LOCAL DIURNAL MAXIMUM MOON ELEVATION '
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then         ! report all
            call reportMaxMoonElevation(
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MAXMEL.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMaxMoonElevation(
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MAXMEL.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMaxMoonElevation(
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MAXMEL.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.830) THEN !  'DETECT LOCAL DIURNAL MINIMUM MOON ELEVATION  -MOONMINEL[0]%2&3&$'
         event100='LOCAL DIURNAL MINIMUM MOON ELEVATION '
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then         ! report all
            call reportMinMoonElevation(
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MINMEL.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMinMoonElevation(
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MINMEL.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMinMoonElevation(
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MINMEL.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.840) THEN !  'DETECT LOCAL DIURNAL MOON AZIMUTH (0=NORTH, 90=EAST) -MOONAZI[*]%2&3&$'
         if (neventVal.lt.4) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 4 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         moonazi=eventVal(4)
         event100=""
         lend=0
         write(event100(lend+1:100),'(F5.1)')
     &        moonazi
         call chop0(event100,100)
         lend=length(event100,100,10)
         event100="LOCAL DIURNAL MOON AZIMUTH ("//
     &        event100(1:lend)//")"
         if (bdeb)write(*,*)myname,event100
         call chop0(event100,100)
         if (lall) then         ! report all
            call reportMoonAzimuth(moonazi,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MAZI.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchMoonAzimuth(moonazi,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MAZI.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchMoonAzimuth(moonazi,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MAZI.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.900) THEN !  'DETECT LOCAL POLAR DAY START -POLDAYSTART[0]%2&3&$'
         event100='LOCAL POLAR SUN DAY START'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lat.gt.0.0D0) then
            Target=-lat - refraction ! target polar angle
         else
            Target=180.0D0+lat - refraction ! target polar angle
         end if
         inrep=nrep+1
         if (lall) then         ! report all
            call reportPolarSunAngle(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTART.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchPolarSunAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLDAYSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchPolarSunAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLDAYSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         jnrep=nrep
         call chop0(event100,100)
         lene=length(event100,100,20)
         event100=event100(1:lene)//' (PREVIOUS SUN RISE)'
         do jj=inrep,jnrep      ! add previous sun rise
            target=-refraction  ! target elevation
            tJD=searchSunUpperElevation(target,
     &           lat,lon,hgt,
     &           repJD(jj)+0.1D0,secdec,
     &           eventId+1,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           -1,            ! search for previous event
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTART.'
               call chop0(crc250,250)
               return
            end if
         end do
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)+1.0D-10
         end do
      ELSE IF (eventId.eq.910) THEN !  'DETECT LOCAL POLAR DAY STOP -POLDAYSTOP[0]%2&3&$'
         event100='LOCAL POLAR SUN DAY STOP'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lat.gt.0.0D0) then
            Target=lat + refraction ! target polar angle
         else
            Target=-180.0D0-lat + refraction ! target polar angle
         end if
         inrep=nrep+1
         if (lall) then         ! report all
            call reportPolarSunAngle(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTOP.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchPolarSunAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLDAYSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchPolarSunAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLDAYSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         call chop0(event100,100)
         lene=length(event100,100,20)
         event100=event100(1:lene)//' (NEXT SUN SET)'
         jnrep=nrep
         do jj=inrep,jnrep      ! add next sun set
            target=-refraction  ! target elevation
            tJD=searchSunUpperElevation(target,
     &           lat,lon,hgt,
     &           repJD(jj)-0.1D0,secdec,
     &           eventId+1,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! only look for decreasing elevation
     &           +1,            ! search for next event
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTOP.'
               call chop0(crc250,250)
               return
            end if
         end do
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-1.0D-10
         end do
      ELSE IF (eventId.eq.920) THEN !  'DETECT LOCAL POLAR NIGHT START -POLNIGHTSTART[0]%2&3&$'
         event100='LOCAL POLAR SUN NIGHT START'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lat.gt.0.0D0) then
            Target=180.0D0 - lat + refraction ! target polar angle
         else
            Target=lat + refraction ! target polar angle
         end if
         inrep=nrep+1
         if (lall) then         ! report all
            call reportPolarSunAngle(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLNIGHTSTART.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchPolarSunAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLNIGHTSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchPolarSunAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLNIGHTSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         call chop0(event100,100)
         lene=length(event100,100,20)
         event100=event100(1:lene)//' (PREVIOUS SUN SET)'
         jnrep=nrep
         do jj=inrep,jnrep      ! add previous sun set
            target=-refraction  ! target elevation
            tJD=searchSunUpperElevation(target,
     &           lat,lon,hgt,
     &           repJD(jj)+0.1D0,secdec,
     &           eventId+1,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! only look for decreasing elevation
     &           -1,            ! search for previous event
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTOP.'
               call chop0(crc250,250)
               return
            end if
         end do
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)+5.0D-10
         end do
      ELSE IF (eventId.eq.930) THEN !  'DETECT LOCAL POLAR NIGHT STOP -POLNIGHTSTOP[0]%2&3&$'
         event100='LOCAL POLAR SUN NIGHT STOP'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lat.gt.0.0D0) then
            Target=-180.0D0 + lat - refraction ! target polar angle
         else
            Target= - lat - refraction ! target polar angle
         end if
         inrep=nrep+1
         if (lall) then         ! report all
            call reportPolarSunAngle(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLNIGHTSTOP.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchPolarSunAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLNIGHTSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchPolarSunAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLNIGHTSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         call chop0(event100,100)
         lene=length(event100,100,20)
         event100=event100(1:lene)//' (NEXT SUN RISE)'
         jnrep=nrep
         do jj=inrep,jnrep      ! add next sun rise
            target=-refraction  ! target elevation
            tJD=searchSunUpperElevation(target,
     &           lat,lon,hgt,
     &           repJD(jj)-0.1D0,secdec,
     &           eventId+1,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           +1,            ! search for next event
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTOP.'
               call chop0(crc250,250)
               return
            end if
         end do
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-5.0D-10
         end do
      ELSE IF (eventId.eq.940) THEN !  'DETECT LOCAL POLAR LUNAR DAY START -POLDAYSTART[0]%2&3&$'
         event100='LOCAL POLAR LUNAR DAY START'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lat.gt.0.0D0) then
            Target=-lat - refraction ! target polar angle
         else
            Target=180.0D0+lat - refraction ! target polar angle
         end if
         write(*,*)myname,'Target:',target,lat
         inrep=nrep+1
         if (lall) then         ! report all
            call reportPolarMoonAngle(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTART.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchPolarMoonAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLDAYSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchPolarMoonAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLDAYSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         jnrep=nrep
         call chop0(event100,100)
         lene=length(event100,100,20)
         event100=event100(1:lene)//' (PREVIOUS MOON RISE)'
         do jj=inrep,jnrep      ! add previous moon rise
            target=-refraction  ! target elevation
            tJD=searchMoonUpperElevation(target,
     &           lat,lon,hgt,
     &           repJD(jj)+0.1D0,secdec,
     &           eventId+1,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           -1,            ! search for previous event
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTART.'
               call chop0(crc250,250)
               return
            end if
         end do
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)+5.0D-10
         end do
      ELSE IF (eventId.eq.950) THEN !  'DETECT LOCAL POLAR LUNAR DAY STOP -POLDAYSTOP[0]%2&3&$'
         event100='LOCAL POLAR LUNAR DAY STOP'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lat.gt.0.0D0) then
            Target=lat + refraction ! target polar angle
         else
            Target=-180.0D0-lat + refraction ! target polar angle
         end if
         inrep=nrep+1
         if (lall) then         ! report all
            call reportPolarMoonAngle(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTOP.'
               call chop0(crc250,250)
               return
            end if
            do jj=inrep,nrep
               repJD(jj)=repJD(jj)-1.0D-10
            end do
         else
            if (lprev) then
               tJD=searchPolarMoonAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLDAYSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchPolarMoonAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLDAYSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         call chop0(event100,100)
         lene=length(event100,100,20)
         event100=event100(1:lene)//' (NEXT MOON SET)'
         jnrep=nrep
         do jj=inrep,jnrep      ! add next moon set
            target=-refraction  ! target elevation
            tJD=searchMoonUpperElevation(target,
     &           lat,lon,hgt,
     &           repJD(jj)-0.1D0,secdec,
     &           eventId+1,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! only look for decreasing elevation
     &           +1,            ! search for next event
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLDAYSTOP.'
               call chop0(crc250,250)
               return
            end if
         end do
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-5.0D-10
         end do
      ELSE IF (eventId.eq.960) THEN !  'DETECT LOCAL POLAR LUNAR NIGHT START -POLNIGHTSTART[0]%2&3&$'
         event100='LOCAL POLAR LUNAR NIGHT START'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lat.gt.0.0D0) then
            Target=180.0D0-lat + refraction ! target polar angle
         else
            Target=lat + refraction ! target polar angle
         end if
         inrep=nrep+1
         if (lall) then         ! report all
            call reportPolarMoonAngle(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLNIGHTSTART.'
               call chop0(crc250,250)
               return
            end if
            do jj=inrep,nrep
               repJD(jj)=repJD(jj)+1.0D-10
            end do
         else
            if (lprev) then
               tJD=searchPolarMoonAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLNIGHTSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchPolarMoonAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLNIGHTSTART.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         call chop0(event100,100)
         lene=length(event100,100,20)
         event100=event100(1:lene)//' (PREVIOUS MOON SET)'
         jnrep=nrep
         do jj=inrep,jnrep      ! add previous sun set
            target=-refraction  ! target elevation
            tJD=searchMoonUpperElevation(target,
     &           lat,lon,hgt,
     &           repJD(jj)+0.1D0,secdec,
     &           eventId+1,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           -1,            ! only look for decreasing elevation
     &           -1,            ! search for previous event
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLNIGHTSTART.'
               call chop0(crc250,250)
               return
            end if
         end do
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)+5.0D-10
         end do
      ELSE IF (eventId.eq.970) THEN !  'DETECT LOCAL POLAR LUNAR NIGHT STOP -POLNIGHTSTOP[0]%2&3&$'
         event100='LOCAL POLAR LUNAR NIGHT STOP'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lat.gt.0.0D0) then
            Target=-180.0D0 + lat - refraction ! target polar angle
         else
            Target=-lat - refraction ! target polar angle
         end if
         inrep=nrep+1
         if (lall) then         ! report all
            call reportPolarMoonAngle(Target,
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLNIGHTSTOP.'
               call chop0(crc250,250)
               return
            end if
            do jj=inrep,nrep
               repJD(jj)=repJD(jj)-1.0D-10
            end do
         else
            if (lprev) then
               tJD=searchPolarMoonAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,         ! search for previous event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLNIGHTSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchPolarMoonAngle(Target,
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,         ! search for next event
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from POLNIGHTSTOP.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         call chop0(event100,100)
         lene=length(event100,100,20)
         event100=event100(1:lene)//' (NEXT MOON RISE)'
         jnrep=nrep
         do jj=inrep,jnrep      ! add previous sun set
            target=-refraction  ! target elevation
            tJD=searchMoonUpperElevation(target,
     &           lat,lon,hgt,
     &           repJD(jj)-0.1D0,secdec,
     &           eventId+1,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           +1,            ! only look for increasing elevation
     &           +1,            ! search for next event
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from POLNIGHTSTOP.'
               call chop0(crc250,250)
               return
            end if
         end do
         do jj=inrep,nrep
            repJD(jj)=repJD(jj)-5.0D-10
         end do
      ELSE IF (eventId.eq.980) THEN !  'DETECT LOCAL SOLAR ECLIPSE (MINOCC MAXOCC)'
         if (neventVal.lt.5) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 5 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         soccmin=eventVal(4)
         soccmax=eventVal(5)
         lsoccmin=.true.
         lsoccmax=.true.
         event100=""
         lend=0
         if (lsoccMin) then
            write(event100(lend+1:100),'(" MINOCC=",F5.1,"%")')
     &           soccmin
            call chop0(event100,100)
            lend=length(event100,100,10)
         end if
         if (lsoccmax) then
            write(event100(lend+1:100),'(" MAXOCC=",F5.1,"%")')
     &           soccmax
            call chop0(event100,100)
            lend=length(event100,100,10)
         end if
         if (lend.ne.0) then
            event100="SOLAR ECLIPSE ("//event100(1:lend)//")"
         else
            event100="SOLAR ECLIPSE"
         end if
         if (bdeb)write(*,*)myname,event100
         call chop0(event100,100)
         if (lall) then         ! report all
            call reportSunEclipse(
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           lsoccmax,soccmax,
     &           lsoccmin,soccmin,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from SUNECLIPSE.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunEclipse(
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              lsoccmax,soccmax,
     &              lsoccmin,soccmin,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SUNECLIPSE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunEclipse(
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              lsoccmax,soccmax,
     &              lsoccmin,soccmin,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SUNECLIPSE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.990) THEN !  'DETECT LOCAL PARTIAL SOLAR ECLIPSE BY MOON STOP -ECLIPSEPARTSTOP[0]%2&3&$'
         event100='SOLAR ECLIPSE'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.3) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 3 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lsoccmin=.false.
         lsoccmax=.false.
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         if (lall) then         ! report all
            call reportSunEclipse(
     &           lat,lon,hgt,
     &           tstartJD,tendJD,secdec,
     &           lsoccmax,soccmax,lsoccmin,soccmin,
     &           eventId,event100,
     &           maxrep,nrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from SUNECLIPSE.'
               call chop0(crc250,250)
               return
            end if
         else
            if (lprev) then
               tJD=searchSunEclipse(
     &              lat,lon,hgt,
     &              tstartJD-dt,secdec,
     &              lsoccmax,soccmax,lsoccmin,soccmin,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              -1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SUNECLIPSE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (lnext) then
               tJD=searchSunEclipse(
     &              lat,lon,hgt,
     &              tstartJD+dt,secdec,
     &              lsoccmax,soccmax,lsoccmin,soccmin,
     &              eventId,event100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              +1,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SUNECLIPSE.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
      ELSE IF (eventId.eq.1000) THEN !  'REPORT POSITIONS AT TIME INCREMENT'
         event100='LOCAL SOLAR SYSTEM POSITIONS'
         if (bdeb)write(*,*)myname,event100
         if (neventVal.lt.4) then
            write(buff250,'(" < ",A," > EventId: ",I4,'//
     &           '" Expected 4 event values, got: ",I4)')
     &           myname,eventId,neventVal
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=842
            return
         end if
         lat=eventVal(1)
         lon=eventVal(2)
         hgt=eventVal(3)
         dtss=eventVal(4)
         call reportSSPos(
     &        lat,lon,hgt,
     &        tstartJD,tendJD,dtss,secdec,
     &        eventId,event100,
     &        maxrep,nrep,repJD,repid,repval,rep250,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from sunPos.'
            call chop0(crc250,250)
            return
         end if
      ELSE
         write(id10,'(I10)')eventId
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Invalid eventId.'//id10
         call chop0(crc250,250)
         irc=100
         return
      END IF
      if (bdeb)write(*,*)myname,"Done.",irc
C
      return
      contains
      subroutine reportJupiterLon(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report id
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'reportJupiterLon'/
      external getJupiterLon
      real getJupiterLon
      real targetCycle
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=40.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getJupiterLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportJupiterLon
      subroutine reportLLMax(
     &     funk,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real lat,lon,hgt
      real funk
      external funk
      real tstartJD
      real tendJD
      real dtin
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*16 myname
      data myname/'reportLLMax'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,fact,dt
      real JD,val,val1,maxval,tol
      parameter (tol=1.0D-6)
      real xJD(3),xval(3),xa,xb,xc
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent,npts
      externale length
      logical bdeb
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      real vals,vale
      integer inrep
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      JD=tstartJD-dt
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,dtin
      npts=0
      inrep=nrep
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     ((dt.gt.0.0D0.and.JD.le.tendJD+2.0D0*dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+2.0D0*dt)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C     need 3 points before we can check for maximum
         xJD(1)=xJD(2)
         xJD(2)=xJD(3)
         xJD(3)=JD
         xval(1)=xval(2)
         xval(2)=xval(3)
         xval(3)=val
         npts=npts+1
         if (npts.ge.3) then
            if (xval(2).gt.xval(1).and.xval(2).gt.xval(3)) then
               xa=xJD(1)
               xb=xJD(2)
               xc=xJD(3)
               maxval=goldenLLMax(xa,xb,xc,
     &              funk,tol,JD,lat,lon,hgt,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from GoldenLLMax.'
                  call chop0(crc250,250)
                  return
               end if
               if ((dt.gt.0.0.and.JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0.and.JD.ge.tendJD.and.
     &              JD.le.tstartJD)) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  nrep=nrep+1
                  if (nrep.le.maxrep) then ! make report
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*fact))
     &                       /fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A," (Max=",F17.5,")")')
     &                    T30(1:lent),event100(1:lend),maxval
                     repJD(nrep)=JD
                     repid(nrep)=eventid
                     repval(nrep)=maxval
                     if (bdeb)write(*,*)myname,'Ended minor loop.'
                  end if
               end if
               JD=xJD(3)
            end if
         end if
         JD=JD+dt         !  increment time
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.eq.inrep) then   ! no local minimum, check end points
         vals=funk(tstartJD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         vale=funk(tendJD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         if (vals.ge.vale) then
            JD=tstartJD
            maxval=vals
         else
            JD=tendJD
            maxval=vale
         end if
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         nrep=nrep+1
         if (nrep.le.maxrep) then ! make report
            if (secdec.gt.0) then
               write(fmt100,'(A,I2.2,A,I1,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2,F",
     &              secdec+1,".",secdec,")"
               fact=10.0D0**(real(secdec))
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1),real(int((sec1-int(sec1))*fact))
     &              /fact
            else
               write(fmt100,'(A,I2.2,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2)"
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1)
            end if
            call chop0(T30,30)
            lent=length(T30,30,30)
            write(rep250(nrep),'(A,X,A," (",F17.5,")")')
     &           T30(1:lent),event100(1:lend),maxval
            repJD(nrep)=JD
            repid(nrep)=eventId
            repval(nrep)=maxval
            if (bdeb)write(*,*)myname,'Ended minor loop.'
         end if
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end subroutine reportLLMax

      subroutine reportLLMin( 
     &     funk,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real lat,lon,hgt
      real funk
      external funk
      real tstartJD
      real tendJD
      real dtin
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*12 myname
      data myname/'reportLLMin'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,fact,dt
      real JD,val,val1,minval,tol
      parameter (tol=1.0D-6)
      real xJD(3),xval(3),xa,xb,xc
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent,npts
      externale length
      logical bdeb
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      real vals,vale
      integer inrep
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      JD=tstartJD-dt
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,dtin
      inrep=nrep
      npts=0
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     ((dt.gt.0.0D0.and.JD.le.tendJD+2.0D0*dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+2.0D0*dt)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C     need 3 points before we can check for minimum
         xJD(1)=xJD(2)
         xJD(2)=xJD(3)
         xJD(3)=JD
         xval(1)=xval(2)
         xval(2)=xval(3)
         xval(3)=val
         npts=npts+1
         if (npts.ge.3) then
            if (xval(2).lt.xval(1).and.xval(2).lt.xval(3)) then
               xa=xJD(1)
               xb=xJD(2)
               xc=xJD(3)
               minval=goldenLLMin(xa,xb,xc,
     &              funk,tol,JD,lat,lon,hgt,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from GoldenLLMin.'
                  call chop0(crc250,250)
                  return
               end if
               if ((dt.gt.0.0.and.JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0.and.JD.ge.tendJD.and.
     &              JD.le.tstartJD)) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  nrep=nrep+1
                  if (nrep.le.maxrep) then ! make report
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*fact))
     &                       /fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A," (Min=",F17.5,")")')
     &                    T30(1:lent),event100(1:lend),minval
                     repJD(nrep)=JD
                     repid(nrep)=eventid
                     repval(nrep)=minval
                     if (bdeb)write(*,*)myname,'Ended minor loop.',idt
                  end if
               end if
               JD=xJD(3)
            end if
         end if
         JD=JD+dt         !  increment time
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.eq.inrep) then ! no local minimum, check end points
         vals=funk(tstartJD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         vale=funk(tendJD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         if (vals.le.vale) then
            JD=tstartJD
            minval=vals
         else
            JD=tendJD
            minval=vale
         end if
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         nrep=nrep+1
         if (nrep.le.maxrep) then ! make report
            if (secdec.gt.0) then
               write(fmt100,'(A,I2.2,A,I1,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2,F",
     &              secdec+1,".",secdec,")"
               fact=10.0D0**(real(secdec))
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1),real(int((sec1-int(sec1))*fact))
     &              /fact
            else
               write(fmt100,'(A,I2.2,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2)"
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1)
            end if
            call chop0(T30,30)
            lent=length(T30,30,30)
            write(rep250(nrep),'(A,X,A," (",F17.5,")")')
     &           T30(1:lent),event100(1:lend),minval
            repJD(nrep)=JD
            repid(nrep)=eventId
            repval(nrep)=minval
            if (bdeb)write(*,*)myname,'Ended minor loop.'
         end if
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=458
         return
      end if
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end subroutine reportLLMin

      subroutine reportLLTarget(tval,
     &     tValCycle,funk,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      implicit none
      real tVal                 ! target value
      real tValCycle            ! target value cycle (360.0D0 for degrees)
      real funk                 ! function giving the value
      external funk             ! 
      real lat,lon,hgt          ! observer latitude (deg), longtitude (deg), height (not used)
      real tstartJD           ! start time in Julian day JD
      real tendJD             ! end time in Julian day JD
      real dtin                 ! time interval (absolute value)
      integer secdec            ! number of decimals in seconds output
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! maximum number of reports
      integer nrep              ! number of reports so far
      real repJD(max(1,maxrep)) ! report time in Julian day JD
      integer repid(max(1,maxrep)) ! report id
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! report strings
      integer pitch             ! 1: rising, -1: falling, 0:any
      character*250 crc250      ! error description
      integer irc               ! error return code (0: ok)
      integer lencrc
C
      character*16 myname
      data myname/'reportLLTarget'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      real ndt
      integer length,lend,lent
      externale length
      real diffcycle
      external diffcycle
      logical bdeb
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      JD=tstartJD
      mindt=(10.0D0**(-real(secdec)))/(3.33D0*86400D0)
      ndt=nint(0.50001D0+abs(tendJD-tstartJD)/max(1.0D-5,abs(dtin)))
      dt=abs(tendJD-tstartJD)/ndt
      dt=sign(max(6.9D-3,abs(dt)),tendJD-tstartJD)
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,mindt,dtin
      first=.true.
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     ((dt.gt.0.0D0.and.JD.le.tendJD+dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+dt)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C
         if (bdeb) write(*,'(4(X,A),4(X,F16.8))')
     &        myname,'Step:',T16,event100(1:lend),val,tval,
     &        dval(1),dval(2)
C         write(*,*)myname,'JNK:',xval(1),xval(2),val
C
C     set boundaries (max/min)
C
         if (first.or.major) then
            xJD(1)=xJD(2)
            xJD(2)=JD
            xval(1)=xval(2)
            xval(2)=val
         end if
C
         dval(1)=diffCycle(xval(1),tval,tValCycle)
         dval(2)=diffCycle(xval(2),tval,tValCycle)
         dval(3)=diffCycle(val,tval,tValCycle)
C     
C     determine if we are in major/minor loop, and increment time
C
         if (first) then
            first=.false.
            idt=dt
            major=.true.        ! we always start in major loop
         else if (major) then   ! check if we must switch to minor loop
            major= (pitch.eq.-1.and.dval(2).gt.0.0D0).or.
     &           (pitch.eq.1.and.dval(1).gt.0.0D0) .or.
     &           ((dval(1).gt.0.0D0 .and. dval(2).gt.0.0D0).or.
     &           (dval(1).lt.0.0D0 .and. dval(2).lt.0.0D0)).or.
     &           (abs(dval(1)-dval(2)).gt.tValCycle*0.75D0)

            if (.not.major) then ! check if we have a cycle change
            end if
            if (.not.major) then ! we have just switched to minor loop
               if (bdeb)write(*,*)myname,'Entered minor loop.',dval(1),
     &              dval(2),dval(3),idt,xval(1),xval(2),val,tval
               xJD(3)=xJD(2)
               xval(3)=xval(2)
               idt= -0.5D0 * (xJD(2)-xJD(1))


            end if
         else if (abs(idt).lt.mindt) then ! we have completed the minor loop
            if (abs(dval(1)-dval(2)).lt.tValCycle*0.25D0 ! check if discontinuity-hit
     &           .or.tValCycle.lt.0.0D0) then ! check if discontinuity-hit
C     make report
               if ( (dt.gt.0.0D0.and.
     &              JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0D0.and.
     &              JD.ge.tendJD.and.
     &              JD.le.tstartJD) ) then
                  nrep=nrep+1
                  if (nrep.le.maxrep) then
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*
     &                       fact))/fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A)')T30(1:lent),
     &                    event100(1:lend)
                     repJD(nrep)=JD
                     repid(nrep)=eventid
                     repval(nrep)=-99999.0D0
                  end if
                  if (bdeb)write(*,*)myname,'Ended minor loop.',idt
               end if
            else if (bdeb) then
               write(*,*)myname,'False hit:',dval(1),dval(2),tValCycle
            end if
            
C     reset variables and continue search
            major=.true.
            xJD(2)=xJD(3)
            xval(2)=xval(3)
            JD=xJD(3)
            idt=dt
         else                   ! we are in minor loop
C            write(*,*)myname,'Minor before:',dval(1),dval(2),dval(3),idt
            if ((dval(3).gt.0.0D0.and.dval(1).gt.0.0D0).or.
     &           (dval(3).lt.0.0D0.and.dval(1).lt.0.0D0)) then
               xJD(1)=xJD(2) ! swap
               xval(1)=xval(2)
               dval(1)=dval(2)
            end if
            xJD(2)=JD
            xval(2)=val
            dval(2)=dval(3)
            idt= -0.5D0 * (xJD(2)-xJD(1)) ! go back half-a-step
C            write(*,*)myname,'Minor  after:',dval(1),dval(2),dval(3),idt
         end if
C     increment time
         JD=JD+idt
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=459
         return
      end if
C
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end subroutine reportLLTarget
C
      subroutine reportLunarTime(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Lunar Time (0-24)
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100    ! event description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report time in Julian day JD
      real repval(max(1,maxrep)) ! report time in Julian day JD
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportLunarTime'/
      external getLunarTime
      real targetCycle,getLunarTime
      parameter (targetCycle=24.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=0.1D0
      call chop0(event100,100)
C
      call reportLLTarget(Target,
     &     TargetCycle,getLunarTime,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportLunarTime
      subroutine reportMarsLon(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of mars celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! report strings
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'reportMarsLon'/
      external getMarsLon
      real targetCycle, getMarsLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=80.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getMarsLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportMarsLon
      subroutine reportMax(
     &     funk,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real funk
      external funk
      real tstartJD
      real tendJD
      real dtin
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*12 myname
      data myname/'reportMax'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,fact,dt
      real JD,val,val1,maxval,tol
      parameter (tol=1.0D-6)
      real xJD(3),xval(3),xa,xb,xc
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent,npts
      externale length
      logical bdeb
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      JD=tstartJD-dt
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,dtin
      npts=0
      cnt=0
      do while (cnt.lt.cntmax.and.(
     &     (dt.gt.0.0D0.and.JD.le.tendJD+2.0D0*dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+2.0D0*dt)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C     need 3 points before we can check for maximum
         xJD(1)=xJD(2)
         xJD(2)=xJD(3)
         xJD(3)=JD
         xval(1)=xval(2)
         xval(2)=xval(3)
         xval(3)=val
         npts=npts+1
         if (npts.ge.3) then
            if (xval(2).gt.xval(1).and.xval(2).gt.xval(3)) then
               xa=xJD(1)
               xb=xJD(2)
               xc=xJD(3)
               maxval=goldenMax(xa,xb,xc,
     &              funk,tol,JD,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from GoldenMax.'
                  call chop0(crc250,250)
                  return
               end if
               if ((dt.gt.0.0.and.JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0.and.JD.ge.tendJD.and.
     &              JD.le.tstartJD)) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  nrep=nrep+1
                  if (nrep.le.maxrep) then ! make report
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*fact))
     &                       /fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A," (Max=",F17.5,")")')
     &                    T30(1:lent),event100(1:lend),maxval
                     repJD(nrep)=JD
                     repid(nrep)=eventid
                     repval(nrep)=maxval
                     if (bdeb)write(*,*)myname,'Ended minor loop.',idt
                  end if
               end if
               JD=xJD(3)
            end if
         end if
         JD=JD+dt         !  increment time
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=460
         return
      end if
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end subroutine reportMax
      subroutine reportMaxMercElong(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMaxMercElong'/
      external getMercElong
      real getMercElong
      real dtin                 ! initial step length
C
      dtin=15.0D0
      call chop0(event100,100)
C
      call reportMax(
     &     getMercElong,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMax.'
         call chop0(crc250,250)
         return
      end if
      return
      end subroutine reportMaxMercElong

      subroutine reportMaxMoonElevation(
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real lat,lon,hgt
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMaxMoonElevation'/
      external getMoonElevation
      real getMoonElevation
      real dtin                 ! initial step length
C
      dtin=0.02D0
      call chop0(event100,100)
C
      call reportLLMax(
     &     getMoonElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMaxMoonElevation

      subroutine reportMaxMoonIllum(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMaxMoonIllum'/
      external getMoonIllum
      real getMoonIllum
      real dtin                 ! initial step length
C
      dtin=3.0D0
C
      call chop0(event100,100)
C
      call reportMax(
     &     getMoonIllum,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMaxMoonIllum

      subroutine reportMaxMoonEarthDist(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMaxMoonEarthDist'/
      external getMoonEarthDist
      real getMoonEarthDist
C
      dtin=3.0D0
      call chop0(event100,100)
C
      call reportMax(
     &     getMoonEarthDist,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMaxMoonEarthDist

      subroutine reportMaxSunEarthDist(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMaxSunEarthDist'/
      external getSunEarthDist
      real getSunEarthDist
C
      dtin=30.0D0
      call chop0(event100,100)
C
      call reportMax(
     &     getSunEarthDist,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMaxSunEarthDist

      subroutine reportMaxSunElevation(
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real lat,lon,hgt
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      real dtin                 ! initial step length
      character*22 myname
      data myname /'reportMaxSunElevation'/
      external getSunElevation
      real getSunElevation
C
      call chop0(event100,100)
C
      dtin=0.02D0
      call reportLLMax(
     &     getSunElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMaxSunElevation

      subroutine reportMaxVenusElong(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMaxVenusElong'/
      external getVenusElong
      real getVenusElong
      real dtin                 ! initial step length
C
      dtin=60.0D0
      call chop0(event100,100)
C
      call reportMax(
     &     getVenusElong,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMaxVenusElong

      subroutine reportMercRelLon(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'reportMercRelLon'/
      external getMercRelLon
      real targetCycle, getMercRelLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=15.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getMercRelLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportMercRelLon
      subroutine reportMercTransit(
     &     tstartJDin,tendJDin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJDin         ! start time
      real tendJDin           ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'reportMercTransit'/
      external getMercRelLon,getMercTransit
      real targetCycle,getMercRelLon,getMercTransit
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
      real Rm,Rp,xCycle,xJD,pJD,dt
      real tstartJD         ! start time
      real tendJD           ! end time
      integer xrep,maxxrep,yy1,mm1,dd1,hh1,mi1
      real sec1
      character*100 xevent100
      integer xeventId
      integer length,lend
      external length
      character*16 t16
      logical bdone
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      call chop0(event100,100)
C
      Rm =   2440.0D0           ! mercury radius
      bdone=.false.
      Target=0.0D0 ! Penumbra entered at mercury inferior conjunction
      tstartJD=tstartJDin
      dtin=sign(min(150.0D0,abs(tendJDin-tstartJDin)),
     &     tendJDin-tstartJDin)
      tendJD=tstartJDin + dtin
      cnt=0
      do while (cnt.lt.cntmax.and..not.bdone)
C
         cnt=cnt+1
         dt=dtin/10.0D0
         tendJD=tstartJD + dtin
         maxxrep=0
         xrep=0
         tstartJD=searchTarget(Target, ! only get time of 
     &        TargetCycle,getMercRelLon,
     &        tstartJD,tendJD,dt,secdec,
     &        eventId,event100,maxxrep,xrep,repJD,repid,repval,rep250,
     &        0,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from searchTarget.'
            call chop0(crc250,250)
            return
         end if
         if (xrep.gt.0) then    ! found a match, search backwards for penumbra entry

            CALL JD2DATE(tstartJD,
     &           YY1,
     &           MM1,
     &           DD1,
     &           HH1,
     &           MI1,
     &           SEC1) 
            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888        FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16

            Rp=getMercTransit(tstartJD,crc250,irc) ! distance between Merc and earth-sun cone
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from getMercTransit.'
               call chop0(crc250,250)
               return
            end if
C            write(*,*) myname,'Rp:',rp,rm,rp.le.rm
            if (Rp.le.Rm) then  ! Mercury eclipses the Sun
               lend=length(event100,100,10)
               dt=10.0D0       ! eclipse starts not more than 10 day earlier
               xeventId=eventId + 1
               xevent100=event100(1:lend)//' STOP'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=min(max(tstartJDin,tendJDin),tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getMercTransit,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
               dt=-10.0D0       ! eclipse starts not more than 10 day earlier
               xeventId=eventId + 0
               xevent100=event100(1:lend)//' START'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=max(min(tstartJDin,tendJDin),tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getMercTransit,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
C               write(*,*) 'REPORTMP:',t16,xJD,nrep,rm
            end if
         end if
         if ((dtin.gt.0.0D0.and.tstartJD+0.5*dtin
     &        .gt.tendJDin).or.
     &        (dtin.lt.0.0D0.and.tstartJD+0.5*dtin
     &        .lt.tendJDin)) then
            bdone=.true.
         else
            tstartJD=tstartJD+dtin*0.5D0 ! skip fast forward
         end if
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      return
      end subroutine reportMercTransit
C
C
      subroutine reportMin(
     &     funk,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real funk
      external funk
      real tstartJD
      real tendJD
      real dtin
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*12 myname
      data myname/'reportMin'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,fact,dt
      real JD,val,val1,minval,tol
      parameter (tol=1.0D-6)
      real xJD(3),xval(3),xa,xb,xc
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent,npts
      externale length
      logical bdeb
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      JD=tstartJD-dt
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,dtin
      npts=0
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     ((dt.gt.0.0D0.and.JD.le.tendJD+2.0D0*dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+2.0D0*dt)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C     need 3 points before we can check for minimum
         xJD(1)=xJD(2)
         xJD(2)=xJD(3)
         xJD(3)=JD
         xval(1)=xval(2)
         xval(2)=xval(3)
         xval(3)=val
         npts=npts+1
         if (npts.ge.3) then
            if (xval(2).lt.xval(1).and.xval(2).lt.xval(3)) then
               xa=xJD(1)
               xb=xJD(2)
               xc=xJD(3)
               minval=goldenMin(xa,xb,xc,
     &              funk,tol,JD,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from GoldenMin.'
                  call chop0(crc250,250)
                  return
               end if
               if ((dt.gt.0.0.and.JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0.and.JD.ge.tendJD.and.
     &              JD.le.tstartJD)) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  nrep=nrep+1
                  if (nrep.le.maxrep) then ! make report
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*fact))
     &                       /fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A," (Min=",F17.5,")")')
     &                    T30(1:lent),event100(1:lend),minval
                     repJD(nrep)=JD
                     repid(nrep)=eventid
                     repval(nrep)=minval
                     if (bdeb)write(*,*)myname,'Ended minor loop.',idt
                  end if
               end if
               JD=xJD(3)
            end if
         end if
         JD=JD+dt         !  increment time
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=461
         return
      end if
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end subroutine reportMin
C
      subroutine reportMinMercElong(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinMercElong'/
      external getMercElong
      real getMercElong
      real dtin                 ! initial step length
C
      dtin=15.0D0
      call chop0(event100,100)
C
      call reportMin(
     &     getMercElong,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMinMercElong

      subroutine reportMinMoonElevation(
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real lat,lon,hgt
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMinMoonElevation'/
      external getMoonElevation
      real getMoonElevation
      real dtin                 ! initial step length
C
      dtin=0.02D0
      call chop0(event100,100)
C
      call reportLLMin(
     &     getMoonElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMinMoonElevation

      subroutine reportMinMoonIllum(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMinMoonIllum'/
      external getMoonIllum
      real getMoonIllum
      real dtin                 ! initial step length
C
      dtin=3.0D0
      call chop0(event100,100)
C
      call reportMin(
     &     getMoonIllum,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMinMoonIllum

      subroutine reportMinMoonEarthDist(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMinMoonEarthDist'/
      external getMoonEarthDist
      real getMoonEarthDist
      real dtin                 ! initial step length
C
      dtin=3.0D0
      call chop0(event100,100)
C
      call reportMin(
     &     getMoonEarthDist,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMinMoonEarthDist
      subroutine reportMinSunEarthDist(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'reportMinSunEarthDist'/
      external getSunEarthDist
      real getSunEarthDist
      real dtin                 ! initial step length
C
      dtin=30.0D0
      call chop0(event100,100)
C
      call reportMin(
     &     getSunEarthDist,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMinSunEarthDist

      subroutine reportMinSunElevation(
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real lat,lon,hgt
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      real dtin                 ! initial step length
      character*22 myname
      data myname /'reportMinSunElevation'/
      external getSunElevation
      real getSunElevation
C
      call chop0(event100,100)
C
      dtin=0.02D0
      call reportLLMin(
     &     getSunElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMinSunElevation

      subroutine reportMinVenusElong(
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinVenusElong'/
      external getVenusElong
      real getVenusElong
      real dtin                 ! initial step length
C
      dtin=60.0D0
      call chop0(event100,100)
C
      call reportMin(
     &     getVenusElong,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end subroutine reportMinVenusElong

      subroutine reportMoonAzimuth(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Moon celestial longtitude
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportMoonAzimuth'/
      external getMoonAzimuth
      real targetCycle,getMoonAzimuth
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=0.1D0
      call chop0(event100,100)
C
      call reportLLTarget(Target,
     &     TargetCycle,getMoonAzimuth,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportMoonAzimuth
      subroutine reportMoonIllum(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*16 myname
      data myname /'reportMoonIllum'/
      external getMoonIllum
      real targetCycle, getMoonIllum
      parameter (targetCycle=100.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=3.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getMoonIllum,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportMoonIllum
      subroutine reportMoonInit(
     &     loclat,loclon,lochgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,
     &     repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real loclat
      real loclon
      real lochgt
      real tstartJD
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*14 myname
      data myname/'reportMoonInit'/
C
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent
      externale length
      logical bdeb
      data bdeb/.false./
C
      real moonPos(6)
      real moonCart(3)
      real moonPolar(3)
      real rxy
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
      integer lenv
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
      real angNight, angDay,angle
      real ang,moonAng,atan2deg,getAngle,phase
      real getPolarMoonAngle
      real getMoonElevation,getMoonPhase
      external atan2deg,getAngle
      external getPolarMoonAngle
      external getMoonElevation,getMoonPhase
C
      call chop0(event100,100)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      JD=tstartJD
      CALL JD2DATE(JD,
     &     YY1,
     &     MM1,
     &     DD1,
     &     HH1,
     &     MI1,
     &     SEC1) 
      WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888  FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C     
C     report moon above/below horison
C
      elevation=getMoonElevation(JD,loclat,loclon,lochgt,crc250,irc) ! uppder disk limb
C     
C     polar moon day
C
      angle=getPolarMoonAngle(JD,
     &     loclat,loclon,lochgt,crc250,irc)
      if (loclat.gt.0.0D0) then
         angDay=angle - refraction ! target polar angle
      else
         angDay=180.0D0 - angle - refraction ! target polar angle
      end if
      if (loclat.gt.0.0D0) then
         angNight=180.0D0 - angle + refraction ! target polar angle
      else
         angNight=angle + refraction ! target polar angle
      end if
      write(*,*)myname,'Polar Moon ang:',angDay,angNight,loclat
C
C     get moon phase
C
      phase=getMoonPhase(JD,crc250,irc) ! moon phase
C
C     make report
C
      if (nrep+1.le.maxrep) then
         if (secdec.gt.0) then
            write(fmt100,'(A,I2.2,A,I1,A)')
     &           "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &           "I2.2,':',I2.2,F",
     &           secdec+1,".",secdec,")"
            fact=10.0D0**(real(secdec))
            write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &           int(sec1),real(int((sec1-int(sec1))*
     &           fact))/fact
         else
            write(fmt100,'(A,I2.2,A)')
     &           "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &           "I2.2,':',I2.2)"
            write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &           int(sec1)
         end if
         call chop0(T30,30)
         lent=length(T30,30,30)
      end if
C     make reports
      nrep=nrep+1
      if (nrep.le.maxrep) then
         if (elevation.gt.-refraction) then ! sun is up
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * ABOVE HORISON"
            repval(nrep)=+1.0D0
         else                   ! sun is down
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * BELOW HORISON"
            repval(nrep)=-1.0D0
         end if
         repJD(nrep)=JD
         repid(nrep)=eventid + 0
      end if
      nrep=nrep+1
      if (nrep.le.maxrep) then
         if (abs(loclat).gt.angDay) then ! polar day
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * POLAR DAY"
            repval(nrep)=+1.0D0
         else if (abs(loclat).gt.angNight) then ! polar night
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * POLAR NIGHT"
            repval(nrep)=-1.0D0
         else                   ! neither
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * NO POLAR EFFECT"
            repval(nrep)=0.0D0
         end if
         repJD(nrep)=JD
         repid(nrep)=eventId + 1
      end if
      nrep=nrep+1
      if (nrep.le.maxrep) then
         if (phase.lt.25.0) then
            write(rep250(nrep),'(A,X,A,X,A,F5.1,X,A)')
     &           T30(1:lent),event100(1:lend)," * MOON PHASE=",phase,
     &           " (waxing crescent)"
         else if (phase.lt.50.0) then
            write(rep250(nrep),'(A,X,A,X,A,F5.1,X,A)')
     &           T30(1:lent),event100(1:lend)," * MOON PHASE=",phase,
     &           " (waxing gibbous)"
         else if (phase.lt.75.0) then
            write(rep250(nrep),'(A,X,A,X,A,F5.1,X,A)')
     &           T30(1:lent),event100(1:lend)," * MOON PHASE=",phase,
     &           " (waning gibbous)"
         else
            write(rep250(nrep),'(A,X,A,X,A,F5.1,X,A)')
     &           T30(1:lent),event100(1:lend)," * MOON PHASE=",phase,
     &           " (waning crescent)"
         end if
         call chop0(rep250(nrep),250)
         repval(nrep)=phase
         repJD(nrep)=JD
         repid(nrep)=eventid + 2
      end if
      if (bdeb)write(*,*)myname,'Done.',irc
      return
C
      end subroutine reportMoonInit
      subroutine reportMoonVisible(
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,
     &     repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD
      real tendJD             ! end time
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*18 myname
      data myname/'reportMoonVisible'/
C
      integer YY1,MM1,DD1,HH1,MI1
      real sec1
      real JD
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent
      externale length
      logical bdeb
      data bdeb/.false./
C
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
      real getMoonElevation
      external getMoonElevation
C
      integer ieventId,rnrep,snrep
      real sum,fact
      logical abovee, aboves
      integer ii
      character*100 ievent100
C
      call chop0(event100,100)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
C
C     get initial and final state (above/below horison)
C
      aboves=(getMoonElevation(tstartJD,lat,lon,hgt,crc250,irc)
     &     .gt.-refraction)     ! uppder disk limb above horison
      abovee=(getMoonElevation(tendJD,lat,lon,hgt,crc250,irc)
     &     .gt.-refraction)     ! uppder disk limb above horison
C
C
C     get list of moon-rise
C
      rnrep=nrep
C      if (aboves) then
C         nrep=nrep+1
C         repJD(nrep)=tstartJD
C      end if
      ieventId=eventId+1
      ievent100=event100(1:lend)//" * RISE"
      call reportMoonRise(lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     ieventId,ievent100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from MOONRISE.'
         call chop0(crc250,250)
         return
      end if
C      if (.not.abovee) then
C         nrep=nrep+1
C         repJD(nrep)=tendJD
C      end if
C
C     get list of moon-set
C
      snrep=nrep
      ieventId=eventId+2
      ievent100=event100(1:lend)//" * SET"
C      if (.not.aboves) then
C         nrep=nrep+1
C         repJD(nrep)=tstartJD
C      end if
      call reportMoonSet(lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     ieventId,ievent100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from MOONSET'
         call chop0(crc250,250)
         return
      end if
      if (abovee) then          ! dummy report that will be overwritten
         nrep=nrep+1
         if (nrep.gt.maxrep) then
            write(buff250,*)
     &           ' < '//myname,' > Too many reports.',nrep,maxrep
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            irc=460
            return
         end if
         repJD(nrep)=tendJD
      end if
C
C     do the math....
C
      sum=0.0D0
      if (tstartJD.lt.tendJD) then
         do ii=snrep+1,nrep
            if (repJD(ii).le.tendJD+1.0D-10) then
               sum=sum + (repJD(ii)-tstartJD)
            end if
         end do
         do ii=rnrep+1,snrep
            if (repJD(ii).le.tendJD+1.0D-10) then
               sum=sum - (repJD(ii)-tstartJD)
            end if
         end do
      else
         do ii=snrep+1,nrep
            if (repJD(ii).ge.tendJD-1.0D-10) then
               sum=sum + (tstartJD-repJD(ii))
            end if
         end do
         do ii=rnrep+1,snrep
            if (repJD(ii).ge.tendJD-1.0D-10) then
               sum=sum - (tstartJD-repJD(ii))
            end if
         end do
      end if
      sum=24.0D0*sum ! convert from days to hours
C
C     make report
C
      JD=tendJD
      CALL JD2DATE(JD,
     &     YY1,
     &     MM1,
     &     DD1,
     &     HH1,
     &     MI1,
     &     SEC1) 
      WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888  FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
      if (.not. abovee) then
         nrep=nrep+1
      end if
      if (nrep.le.maxrep) then
         if (secdec.gt.0) then
            write(fmt100,'(A,I2.2,A,I1,A)')
     &           "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &           "I2.2,':',I2.2,F",
     &           secdec+1,".",secdec,")"
            fact=10.0D0**(real(secdec))
            write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &           int(sec1),real(int((sec1-int(sec1))*
     &           fact))/fact
         else
            write(fmt100,'(A,I2.2,A)')
     &           "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &           "I2.2,':',I2.2)"
            write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &           int(sec1)
         end if
         call chop0(T30,30)
         lent=length(T30,30,30)
         write(rep250(nrep),'(A,X,A,X,A,F5.1,X,A)')
     &        T30(1:lent),event100(1:lend)," * VISIBLE=",sum,"h"
         repval(nrep)=sum
         repJD(nrep)=JD
         repid(nrep)=eventid
      end if
      if (bdeb)write(*,*)myname,'Done.',irc
      return
C
      end subroutine reportMoonVisible
      subroutine reportMoonPhase(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*16 myname
      data myname /'reportMoonPhase'/
      external getMoonPhase
      real targetCycle, getMoonPhase
      parameter (targetCycle=100.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=3.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getMoonPhase,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportMoonPhase
      subroutine reportMoonPos(loclat,loclon,lochgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real loclat
      real loclon
      real lochgt
      real tstartJD
      real tendJD
      real dtin
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*14 myname
      data myname/'reportMoonPos'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent
      externale length
      logical bdeb
      data bdeb/.false./
C
      real moonPos(6)
      real moonCart(3)
      real moonPolar(3)
      real rxy
      real rMoon
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,elevation,azimuth,range
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
      integer cnt,cntmax
      parameter (cntmax=10000)
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      real ang,moonAng,atan2deg,getAngle
      external atan2deg,getAngle
C
      if (bdeb)write(*,*)myname,'Entering.',irc
C
      call chop0(event100,100)
C
      lend=length(event100,100,20)
      JD=tstartJD
      mindt=(10.0D0**(-real(secdec)))/(3.33D0*86400D0)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,mindt,dtin
      first=.true.
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     ((dt.gt.0.0D0.and.JD.le.tendJD+dt*0.9999999D0).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+dt*0.9999999D0)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get moon position in local polar coordinates (azimuth, elevation, range)
C
         rMoon     =  1738.0D0
         call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephRead.'
            call chop0(crc250,250)
            return
         end if
C     
         call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephMoonMJ.'
            call chop0(crc250,250)
            return
         end if
C     
         call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephLight.'
            call chop0(crc250,250)
            return
         end if
C     
         call JPLephMJtoEF(JD,moonPos,crc250,irc) ! convert to "Earth Fixed"
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephMJtoTD.'
            call chop0(crc250,250)
            return
         end if
C     
         call JPLephEFtoTC(loclat,loclon,lochgt,moonPos,          ! EARTH FIXED
     &        moonCart,         ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &        moonPolar,        ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &        crc250,irc)
         if(irc.ne.0) then
            write(*,*)myname,'Error return from JPLephEFtoTC.',irc
            return
         end if

C     
         azimuth=moonPolar(3)*DEG
         elevation=moonPolar(2)*DEG
C     &        + (rMoon/moonPolar(1))*DEG ! disk upper limb
         range=moonPolar(1)
C     make report
         if (nrep+1.le.maxrep) then
            if (secdec.gt.0) then
               write(fmt100,'(A,I2.2,A,I1,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2,F",
     &              secdec+1,".",secdec,")"
               fact=10.0D0**(real(secdec))
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1),real(int((sec1-int(sec1))*
     &              fact))/fact
            else
               write(fmt100,'(A,I2.2,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2)"
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1)
            end if
            call chop0(T30,30)
            lent=length(T30,30,30)
         end if
         nrep=nrep+1
         if (nrep.le.maxrep) then
            write(rep250(nrep),'(A,X,A," Elv:",F10.3,'//
     &           '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &           T30(1:lent),event100(1:lend),
     &           elevation,azimuth,range
            repJD(nrep)=JD - 1.0D-9
            repid(nrep)=eventId + 0
            repval(nrep)=elevation
         end if
         nrep=nrep+1
         if (nrep.le.maxrep) then
            write(rep250(nrep),'(A,X,A," Elv:",F10.3,'//
     &           '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &           T30(1:lent),event100(1:lend),
     &           elevation,azimuth,range
            repJD(nrep)=JD - 1.0D-9 + 1.0D-10
            repid(nrep)=eventId + 1
            repval(nrep)=azimuth
         end if
         nrep=nrep+1
         if (nrep.le.maxrep) then
            write(rep250(nrep),'(A,X,A," Elv:",F10.3,'//
     &           '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &           T30(1:lent),event100(1:lend),
     &           elevation,azimuth,range
            repJD(nrep)=JD - 1.0D-9 + 2.0D-10
            repid(nrep)=eventId + 2
            repval(nrep)=range
         end if
         if (bdeb)write(*,*)myname,'Ended minor loop.',dt
C     increment time
         JD=JD+dt
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=462
         return
      end if
C
C
      if (bdeb)write(*,*)myname,'Done.',irc
      return
C
      end subroutine reportMoonPos
      subroutine reportMoonRise(lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dt                   ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportMoonRise'/
      external getMoonElevation
      real getMoonElevation
C
      real target               ! target value of Moon celestial longtitude
      integer jj,inrep
      real targetCycle
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
C
      Target= -refraction       ! target elevation
      dt=0.1D0                  ! initial time increment (days)
      inrep=nrep+1
C     
      call chop0(event100,100)
C     
      call reportLLTarget(Target,
     &     TargetCycle,getMoonElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dt,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     1,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C     
      do jj=inrep,nrep
         repJD(jj)=repJD(jj)-1.0D-10
      end do
      
      return
      end subroutine reportMoonRise
      subroutine reportMoonSet(lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dt                   ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportMoonSet'/
      external getMoonElevation
      real getMoonElevation
C
      real target               ! target value of Moon celestial longtitude
      integer jj,inrep
      real targetCycle
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
C
      Target= -refraction       ! target elevation
      dt=0.1D0                  ! initial time increment (days)
      inrep=nrep+1
C     
      call chop0(event100,100)
C     
      call reportLLTarget(Target,
     &     TargetCycle,getMoonElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dt,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     -1,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C     
      do jj=inrep,nrep
         repJD(jj)=repJD(jj)-1.0D-10
      end do
      
      return
      end subroutine reportMoonSet
      subroutine reportMoonUpperElevation(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dtin                   ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer pitch             ! 1: rising, -1: falling, 0:any
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*26 myname
      data myname /'reportMoonUpperElevation'/
      external getMoonUpperElevation
      real targetCycle, getMoonUpperElevation
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
C
      call chop0(event100,100)
C
      dtin=0.02D0
      call reportLLTarget(Target,
     &     TargetCycle,getMoonUpperElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportMoonUpperElevation
      subroutine reportPolarSunAngle(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportPolarSunAngle'/
      external getPolarSunAngleX
      real targetCycle, getPolarSunAngleX
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=30.0D0
      call chop0(event100,100)
C
      call reportLLTarget(Target,
     &     TargetCycle,getPolarSunAngleX,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
      return
      end subroutine reportPolarSunAngle
      subroutine reportPolarMoonAngle(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportPolarMoonAngle'/
      external getPolarMoonAngleX
      real targetCycle, getPolarMoonAngleX
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=30.0D0
      call chop0(event100,100)
C
      call reportLLTarget(Target,
     &     TargetCycle,getPolarMoonAngleX,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
      return
      end subroutine reportPolarMoonAngle
      subroutine reportSaturnLon(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'reportSaturnLon'/
      external getSaturnLon
      real targetCycle, getSaturnLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=40.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getSaturnLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportSaturnLon
      subroutine reportSolarTime(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportSolarTime'/
      external getSolarTime
      real targetCycle, getSolarTime
      parameter (targetCycle=24.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=0.1D0
      call chop0(event100,100)
C
      call reportLLTarget(Target,
     &     TargetCycle,getSolarTime,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportSolarTime
      subroutine reportSSPos(loclat,loclon,lochgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real loclat
      real loclon
      real lochgt
      real tstartJD
      real tendJD
      real dtin
       integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*14 myname
      data myname/'reportSSPos'/
C
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent
      externale length
      logical bdeb
      data bdeb/.false./
      integer ii
      character*10 body10
C
      character*250 buff250
      real pos(6)
      real cart(3)
      real polar(3)
      real rxy
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,elevation,azimuth,range
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      real ang,sunAng,atan2deg,getAngle
      external atan2deg,getAngle
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
C
      call chop0(event100,100)
C
      lend=length(event100,100,20)
      JD=tstartJD
      mindt=(10.0D0**(-real(secdec)))/(3.33D0*86400D0)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,mindt,dtin
      first=.true.
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     ((dt.gt.0.0D0.and.JD.le.tendJD+dt*0.99999D0).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+dt*0.99999D0)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get position in local polar coordinates (azimuth, elevation, range)
C
         call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephRead.'
            call chop0(crc250,250)
            return
         end if
C     
         do ii=1,10
            IF (II.EQ.1) THEN
               body10='Sun       '
               call JPLephSunMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SunMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.2) then
               body10='Mercury   '
               call JPLephMercuryMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MercuryMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.3) then
               body10='Venus     '
               call JPLephVenusMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from VenusMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.4) then
               body10='Moon      '
               call JPLephMoonMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MoonMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.5) then
               body10='Mars      '
               call JPLephMarsMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from MarsMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.6) then
               body10='Jupiter   '
               call JPLephJupiterMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from JupiterMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.7) then
               body10='Saturn   '
               call JPLephSaturnMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from SaturnMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.8) then
               body10='Uranus    '
               call JPLephUranusMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from UranusMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.9) then
               body10='Neptun    '
               call JPLephNeptunMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from NeptunMJ.'
                  call chop0(crc250,250)
                  return
               end if
            else if (ii.eq.10) then
               body10='Pluto    '
               call JPLephPlutoMJ(pos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from PlutoMJ.'
                  call chop0(crc250,250)
                  return
               end if
            END IF
C     
            call JPLephLight(pos,crc250,irc) ! correct for light flight time...
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from Light.'
               call chop0(crc250,250)
               return
            end if
C     
            call JPLephMJtoEF(JD,pos,crc250,irc) ! convert to "Earth Fixed"
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from MJtoEF.'
               call chop0(crc250,250)
               return
            end if
C     
            call JPLephEFtoTC(loclat,loclon,lochgt,pos, ! EARTH FIXED
     &           cart,          ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &           polar,         ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &           crc250,irc)
            if(irc.ne.0) then
               write(*,*)myname,'Error return from JPLephEFtoTC.',irc
               return
            end if
C     
            azimuth=polar(3)*DEG
            elevation=polar(2)*DEG
            range=polar(1)
C     make report
            if (nrep+1.le.maxrep) then
               if (secdec.gt.0) then
                  write(fmt100,'(A,I2.2,A,I1,A)')
     &                 "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                 "I2.2,':',I2.2,F",
     &                 secdec+1,".",secdec,")"
                  fact=10.0D0**(real(secdec))
                  write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                 int(sec1),real(int((sec1-int(sec1))*
     &                 fact))/fact
               else
                  write(fmt100,'(A,I2.2,A)')
     &                 "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                 "I2.2,':',I2.2)"
                  write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                 int(sec1)
               end if
               call chop0(T30,30)
               lent=length(T30,30,30)
            end if
            nrep=nrep+1
            if (nrep.le.maxrep) then
               write(rep250(nrep),'(A,X,A," ",A," Elv:",F10.3,'//
     &              '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &              T30(1:lent),event100(1:lend),body10,
     &              elevation,azimuth,range
               repJD(nrep)=JD + ii*1.0D-9
               repid(nrep)=eventId + (ii-1)*10 + 0
               repval(nrep)=elevation
            end if
            nrep=nrep+1
            if (nrep.le.maxrep) then
               write(rep250(nrep),'(A,X,A," ",A," Elv:",F10.3,'//
     &              '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &              T30(1:lent),event100(1:lend),body10,
     &              elevation,azimuth,range
               repJD(nrep)=JD + ii*1.0D-9 + 1.0D-10
               repid(nrep)=eventId + (ii-1)*10 + 1
               repval(nrep)=azimuth
            end if
            nrep=nrep+1
            if (nrep.le.maxrep) then
               write(rep250(nrep),'(A,X,A," ",A," Elv:",F10.3,'//
     &              '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &              T30(1:lent),event100(1:lend),body10,
     &              elevation,azimuth,range
               repJD(nrep)=JD + ii*1.0D-9 + 2.0D-10
               repid(nrep)=eventId + (ii-1)*10 + 2
               repval(nrep)=range
            end if
            if (bdeb)write(*,*)myname,'Ended minor loop.',dt
         end do
C     increment time
         JD=JD+dt
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=463
         return
      end if
C
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
C     
      end subroutine reportSSPos
      
      subroutine reportSunAzimuth(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportSunAzimuth'/
      external getSunAzimuth
      real targetCycle,getSunAzimuth
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=0.1D0
      call chop0(event100,100)
C
      call reportLLTarget(Target,
     &     TargetCycle,getSunAzimuth,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportSunAzimuth
      subroutine reportSunEclipse(
     &     lat,lon,hgt,
     &     tstartJDin,tendJDin,secdec,
     &     loccMax,occMax,loccMin,occMin,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real lat,lon,hgt
      real tstartJDin         ! start time
      real tendJDin           ! end time
      integer secdec            ! number of second-decimals
      logical loccMax
      real occMax
      logical loccMin
      real occMin
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! event id in reports
      real repval(max(1,maxrep)) ! event value in reports
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C     
      character*18 myname
      data myname /'reportSunEclipse'/
      external getMoonPhase,getSunEclipse,getSunEclipseOcc,getSunOcc
      real getMoonPhase,getSunEclipse,getSunEclipseOcc,getSunOcc
      real targetCycle
      parameter (targetCycle=100.0D0) ! target value which correponds to 0.0D0
      real dtin                 ! initial time increment
      real Rm,Rp,Ru,xCycle,xJD,pJD,dt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer xrep,maxxrep,yy1,mm1,dd1,hh1,mi1
      real sec1
      real target               ! target value of Sun celestial longtitude
      character*100 xevent100
      integer xeventId
      integer length,lend,leno,lent
      external length
      character*16 t16
      character*30 t30
      character*100 fmt100
      logical bdone
      real occ,fact
      logical proceed
      integer cnt,cntmax
      parameter (cntmax=10000)
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
      external getSunElevation
      real getSunElevation
C
      call chop0(event100,100)
      lend=length(event100,100,10)
C
      Rm =  1738.0D0            ! moon radius
      bdone=.false.
      Target=0.0D0              ! SunEclipse entered at new moon
      tstartJD=tstartJDin
      dtin=sign(min(33.0D0,abs(tendJDin-tstartJDin)),
     &     tendJDin-tstartJDin)
      tendJD=tstartJDin + dtin
      cnt=0
      do while (cnt.lt.cntmax.and..not.bdone)
         cnt=cnt+1
         dt=dtin/10.0D0
         tendJD=tstartJD + dtin
         maxxrep=0
         xrep=0
         xJD=searchTarget(Target, ! only get time of 
     &        TargetCycle,getMoonPhase,
     &        tstartJD,tendJD,dt,secdec,
     &        eventId,event100,maxxrep,xrep,repJD,repid,repval,rep250,
     &        0,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from searchTarget.'
            call chop0(crc250,250)
            return
         end if
         if (xrep.gt.0) then    ! found a match, search backwards for sunEclipse entry
            tstartJD=xJD
            maxxrep=0
            xrep=0
            tstartJD=searchLLMax( ! only get time of 
     &           getSunOcc,
     &           lat,lon,hgt,
     &           tstartJD-0.5D0,tstartJD+0.5D0,0.01D0,secdec,
     &           eventId,event100,
     &           maxxrep,xrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from searchLLMax.'
               call chop0(crc250,250)
               return
            end if
            Rp=getSunEclipse(tstartJD,lat,lon,hgt,crc250,irc) ! distance between Moon center and earth-pensunEclipse
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from GetSunEclipse.'
               call chop0(crc250,250)
               return
            end if
            proceed=(Rp.lt.Rm)
            occ=getSunEclipseOcc()
            CALL JD2DATE(tstartJD,
     &           YY1,
     &           MM1,
     &           DD1,
     &           HH1,
     &           MI1,
     &           SEC1) 
            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888        FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
            if (proceed) then
               proceed=(proceed.and.(.not.loccMin.or.occ.ge.occMin))
               proceed=(proceed.and.(.not.loccMax.or.occ.le.occMax))
            end if
C     find out if sun is visible at maximum occultation...
            if (proceed) then
               proceed=(getSunElevation(tstartJD,lat,lon,hgt,
     &              crc250,irc).gt.-refraction) ! uppder disk limb above horison
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from getSunElevation.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (proceed) then  ! we have a solar eclipse
C     search backwards for Rp .eq. Rm (PENSUNECLIPSE PARTIAL START)
               dt=-0.2D0        ! eclipse starts not more than 1 day earlier
               xeventId=eventId + 0
               xevent100=event100(1:lend)//' PARTIAL STARTS'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=max(min(tstartJDin,tendJDin),
     &              tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchLLTarget(Rm, ! find start
     &              xCycle,getSunEclipse,
     &              lat,lon,hgt,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchLLTarget.'
                  call chop0(crc250,250)
                  return
               end if
C     write(*,*) myname,'Ru:',rp,rm,rp.le.rm
               if (Rp.le.-Rm) then               
C     search backwards for Rp .eq. -Rm (TOTAL ECLIPSE START)
                  dt=-0.2D0     ! eclipse starts not more than 1 day earlier
                  xeventId=eventId + 1
                  xevent100=event100(1:lend)//' TOTAL STARTS'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=max(min(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchLLTarget(-Rm, ! find start
     &                 xCycle,getSunEclipse,
     &                 lat,lon,hgt,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchLLTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
C     report max occulation
               nrep=nrep+1
               if (nrep.le.maxrep) then
                  if (secdec.gt.0) then
                     write(fmt100,'(A,I2.2,A,I1,A)')
     &                    "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                    "I2.2,':',I2.2,F",
     &                    secdec+1,".",secdec,")"
                     fact=10.0D0**(real(secdec))
                     write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                    int(sec1),real(int((sec1-int(sec1))*
     &                    fact))/fact
                  else
                     write(fmt100,'(A,I2.2,A)')
     &                    "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                    "I2.2,':',I2.2)"
                     write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                    int(sec1)
                  end if
                  call chop0(T30,30)
                  lent=length(T30,30,30)
                  lend=length(event100,100,30)
                  write(rep250(nrep),
     &                 '(A,X,A," MAX OCCULTATION=",F8.1,"%")')
     &                 T30(1:lent),event100(1:lend),
     &                 occ
                  repJD(nrep)=tstartJD
                  repid(nrep)=eventId + 2
                  repval(nrep)=occ
               end if
               if (Rp.le.-Rm) then ! Moon totally within Sun cone
C     search forwards for Ru .eq. -Rm (TOTAL ECLIPSE STOP)
                  dt=0.2D0      ! eclipse stops not more than 1 day later
                  xeventId=eventId + 3
                  xevent100=event100(1:lend)//
     &                 ' TOTAL STOPS'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=min(max(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchLLTarget(-Rm, ! find stop
     &                 xCycle,getSunEclipse,
     &                 lat,lon,hgt,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchLLTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
C     search forwards for Rp .eq. Rm (PARTIAL ECLIPSE STOP)
               dt=0.2D0         ! eclipse stops not more than 1 day later
               xeventId=eventId + 4
               xevent100=event100(1:lend)//' PARTIAL STOPS'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=min(max(tstartJDin,tendJDin),
     &              tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchLLTarget(Rm, ! find stop
     &              xCycle,getSunEclipse,
     &              lat,lon,hgt,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchLLTarget.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         if ((dtin.gt.0.0D0.and.tstartJD+0.5*dtin
     &        .gt.tendJDin).or.
     &        (dtin.lt.0.0D0.and.tstartJD+0.5*dtin
     &        .lt.tendJDin)) then
            bdone=.true.
         else
            tstartJD=tstartJD+dtin*0.5D0 ! skip fast forward
         end if
      end do
C     
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
C     
      return
      end subroutine reportSunEclipse
      
      subroutine reportSunElevation(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dtin                 ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100    ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer pitch             ! 1: rising, -1: falling, 0:any
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportSunElevation'/
      external getSunElevation
      real targetCycle, getSunElevation
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
C
      call chop0(event100,100)
C
      dtin=0.02D0
      call reportLLTarget(Target,
     &     TargetCycle,getSunElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportSunElevation
      subroutine reportSunInit(loclat,loclon,lochgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real loclat
      real loclon
      real lochgt
      real tstartJD
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*14 myname
      data myname/'reportSunInit'/
C
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent
      externale length
      logical bdeb
      data bdeb/.false./
C
      real sunPos(6)
      real sunCart(3)
      real sunPolar(3)
      real rxy
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,elevation,azimuth,range
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
      integer lenv
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
      real angNight, angDay,angle
      real ang,sunAng,atan2deg,getAngle,
     &     getPolarSunAngle,getSunUpperElevation
      external atan2deg,getAngle,
     &     getPolarSunAngle,getSunUpperElevation
C
      call chop0(event100,100)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      JD=tstartJD
      CALL JD2DATE(JD,
     &     YY1,
     &     MM1,
     &     DD1,
     &     HH1,
     &     MI1,
     &     SEC1) 
      WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888  FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C     
C     sun above/below horison
C
      elevation=getSunUpperElevation(JD,
     &     loclat,loclon,lochgt,crc250,irc) ! ippder disk limb
C     
C     polar sun day
C
      angle=getPolarSunAngle(JD,loclat,loclon,lochgt,crc250,irc)
      if (loclat.gt.0.0D0) then
         angDay=angle - refraction ! target polar angle
      else
         angDay=180.0D0 - angle - refraction ! target polar angle
      end if
      if (loclat.gt.0.0D0) then
         angNight=180.0D0 - angle + refraction ! target polar angle
      else
         angNight=angle + refraction ! target polar angle
      end if
C      
      write(*,*)myname,'Polar Sun ang:',angDay,angNight,loclat
C
C     make report
C
      if (nrep+1.le.maxrep) then
         if (secdec.gt.0) then
            write(fmt100,'(A,I2.2,A,I1,A)')
     &           "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &           "I2.2,':',I2.2,F",
     &           secdec+1,".",secdec,")"
            fact=10.0D0**(real(secdec))
            write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &           int(sec1),real(int((sec1-int(sec1))*
     &           fact))/fact
         else
            write(fmt100,'(A,I2.2,A)')
     &           "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &           "I2.2,':',I2.2)"
            write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &           int(sec1)
         end if
         call chop0(T30,30)
         lent=length(T30,30,30)
      end if
C     make reports
      nrep=nrep+1
      if (nrep.le.maxrep) then
         if (elevation.gt.-refraction) then ! sun is up
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * ABOVE HORISON"
            repval(nrep)=+1.0D0
         else                   ! sun is down
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * BELOW HORISON"
            repval(nrep)=-1.0D0
         end if
         repJD(nrep)=JD
         repid(nrep)=eventId + 0
      end if
      nrep=nrep+1
      if (nrep.le.maxrep) then
         if (abs(loclat).gt.angDay) then ! polar day
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * POLAR DAY"
            repval(nrep)=+1.0D0
         else if (abs(loclat).gt.angNight) then ! polar night
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * POLAR NIGHT"
            repval(nrep)=-1.0D0
         else                   ! neither
            write(rep250(nrep),'(A,X,A,X,A)')
     &           T30(1:lent),event100(1:lend)," * NO POLAR EFFECT"
            repval(nrep)=0.0D0
         end if
         repJD(nrep)=JD + 1.0D-10
         repid(nrep)=eventId + 1
      end if
      if (bdeb)write(*,*)myname,'Ended minor loop.',idt
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end subroutine reportSunInit
      subroutine reportSunVisible(
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,
     &     repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD
      real tendJD             ! end time
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*18 myname
      data myname/'reportSunVisible'/
C
      integer YY1,MM1,DD1,HH1,MI1
      real sec1
      real JD
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent
      externale length
      logical bdeb
      data bdeb/.false./
C
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
      real getSunUpperElevation
      external getSunUpperElevation
C
      integer ieventId,rnrep,snrep
      real sum,fact
      logical abovee, aboves
      integer ii
      character*100 ievent100
C
      call chop0(event100,100)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
C
C     get initial and final state (above/below horison)
C
      aboves=(getSunUpperElevation(tstartJD,lat,lon,hgt,crc250,irc)
     &     .gt.-refraction)     ! uppder disk limb above horison
      abovee=(getSunUpperElevation(tendJD,lat,lon,hgt,crc250,irc)
     &     .gt.-refraction)     ! uppder disk limb above horison
C
      ieventId=0
C
C     get list of sun-rise
C
      rnrep=nrep
C      if (aboves) then
C         nrep=nrep+1
C         repJD(nrep)=tstartJD
C      end if
      ieventId=eventId+1
      ievent100=event100(1:lend)//" * RISE"      
      call reportSunUpperElevation(-refraction,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     ieventId,ievent100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     +1,                  ! rising elevation
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from SUNRISE.'
         call chop0(crc250,250)
         return
      end if
C      if (.not.abovee) then
C         nrep=nrep+1
C         repJD(nrep)=tendJD
C      end if
C      do ii=1,rnrep
C         write(*,*)ii,rep250(ii)(1:60)
C      end do
C
C     get list of sun-set
C
      snrep=nrep
C      if (.not.aboves) then
C         nrep=nrep+1
C         repJD(nrep)=tstartJD
C      end if
      ieventId=eventId+2
      ievent100=event100(1:lend)//" * SET"      
      call reportSunUpperElevation(-refraction,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     ieventId,ievent100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     -1,                  ! decreasing elevation
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from SUNRISE.'
         call chop0(crc250,250)
         return
      end if
      if (abovee) then
         nrep=nrep+1
         repJD(nrep)=tendJD
      end if
C      do ii=1,rnrep
C         write(*,*)ii,irep250(ii)(1:60)
C      end do
C
C     do the math....
C
      sum=0.0D0
      if (tstartJD.lt.tendJD) then
         do ii=snrep+1,nrep
            if (repJD(ii).le.tendJD+1.0D-10) then
               sum=sum + (repJD(ii)-tstartJD)
            end if
         end do
         do ii=rnrep+1,snrep
            if (repJD(ii).le.tendJD+1.0D-10) then
               sum=sum - (repJD(ii)-tstartJD)
            end if
         end do
      else
         do ii=snrep+1,nrep
            if (repJD(ii).ge.tendJD-1.0D-10) then
               sum=sum + (tstartJD-repJD(ii))
            end if
         end do
         do ii=rnrep+1,snrep
            if (repJD(ii).ge.tendJD-1.0D-10) then
               sum=sum - (tstartJD-repJD(ii))
            end if
         end do
      end if
      sum=24.0D0*sum ! convert from days to hours
C
C     make report
C
      JD=tendJD
      CALL JD2DATE(JD,
     &     YY1,
     &     MM1,
     &     DD1,
     &     HH1,
     &     MI1,
     &     SEC1) 
      WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888  FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
      if (.not. abovee) then
         nrep=nrep+1
      end if
      if (nrep.le.maxrep) then
         if (secdec.gt.0) then
            write(fmt100,'(A,I2.2,A,I1,A)')
     &           "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &           "I2.2,':',I2.2,F",
     &           secdec+1,".",secdec,")"
            fact=10.0D0**(real(secdec))
            write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &           int(sec1),real(int((sec1-int(sec1))*
     &           fact))/fact
         else
            write(fmt100,'(A,I2.2,A)')
     &           "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &           "I2.2,':',I2.2)"
            write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &           int(sec1)
         end if
         call chop0(T30,30)
         lent=length(T30,30,30)
         write(rep250(nrep),'(A,X,A,X,A,F5.1,X,A)')
     &        T30(1:lent),event100(1:lend)," * VISIBLE=",sum,"h"
         repval(nrep)=sum
         repJD(nrep)=JD
         repid(nrep)=eventid
      end if
      if (bdeb)write(*,*)myname,'Done.',irc
      return
C
      end subroutine reportSunVisible
      subroutine reportSunPos(loclat,loclon,lochgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real loclat
      real loclon
      real lochgt
      real tstartJD
      real tendJD
      real dtin
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*14 myname
      data myname/'reportSunPos'/
C
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent
      externale length
      logical bdeb
      data bdeb/.false./
C
      real sunPos(6)
      real sunCart(3)
      real sunPolar(3)
      real rxy
      real rSun
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,elevation,azimuth,range
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*250 buff250
      real ang,sunAng,atan2deg,getAngle
      external atan2deg,getAngle
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
C
      call chop0(event100,100)
C
      lend=length(event100,100,20)
      JD=tstartJD
      mindt=(10.0D0**(-real(secdec)))/(3.33D0*86400D0)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,mindt,dtin
      first=.true.
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     ((dt.gt.0.0D0.and.JD.lt.tendJD+dt*0.99999D0).or.
     &     (dt.lt.0.0D0.and.JD.gt.tendJD+dt*0.99999D0)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get sun position in local polar coordinates (azimuth, elevation, range)
C
         rSun     = 682500.0D0
         call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephRead.'
            call chop0(crc250,250)
            return
         end if
C     
         call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephSunMJ.'
            call chop0(crc250,250)
            return
         end if
C     
         call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephLight.'
            call chop0(crc250,250)
            return
         end if
C     
         call JPLephMJtoEF(JD,sunPos,crc250,irc) ! convert to "Earth Fixed"
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from JPLephMJtoTD.'
            call chop0(crc250,250)
            return
         end if
C
         call JPLephEFtoTC(loclat,loclon,lochgt,sunPos,          ! EARTH FIXED
     &        sunCart,         ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &        sunPolar,        ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &        crc250,irc)
         if(irc.ne.0) then
            write(*,*)myname,'Error return from JPLephEFtoTC.',irc
            return
         end if

C     
         azimuth=sunPolar(3)*DEG
         elevation=sunPolar(2)*DEG
C     &        + (rSun/sunPolar(1))*DEG ! disk upper limb
         range=sunPolar(1)
C     make report
         if (nrep+1.le.maxrep) then
            if (secdec.gt.0) then
               write(fmt100,'(A,I2.2,A,I1,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2,F",
     &              secdec+1,".",secdec,")"
               fact=10.0D0**(real(secdec))
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1),real(int((sec1-int(sec1))*
     &              fact))/fact
            else
               write(fmt100,'(A,I2.2,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2)"
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1)
            end if
            call chop0(T30,30)
            lent=length(T30,30,30)
         end if
         nrep=nrep+1
         if (nrep.le.maxrep) then
            write(rep250(nrep),'(A,X,A," Elv:",F10.3,'//
     &           '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &           T30(1:lent),event100(1:lend),
     &           elevation,azimuth,range
            repJD(nrep)=JD - 2.0D-9
            repid(nrep)=eventId + 0
            repval(nrep)=elevation
         end if
         nrep=nrep+1
         if (nrep.le.maxrep) then
            write(rep250(nrep),'(A,X,A," Elv:",F10.3,'//
     &           '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &           T30(1:lent),event100(1:lend),
     &           elevation,azimuth,range
            repJD(nrep)=JD - 2.0D-9 + 1.0D-10
            repid(nrep)=eventId + 1
            repval(nrep)=azimuth
         end if
         nrep=nrep+1
         if (nrep.le.maxrep) then
            write(rep250(nrep),'(A,X,A," Elv:",F10.3,'//
     &           '" deg, Azi:",F10.3,", Rng:",F15.1," km")')
     &           T30(1:lent),event100(1:lend),
     &           elevation,azimuth,range
            repJD(nrep)=JD - 2.0D-9 + 2.0D-10
            repid(nrep)=eventId + 2
            repval(nrep)=range
         end if
         if (bdeb)write(*,*)myname,'Ended minor loop.',dt
C     increment time
         JD=JD+dt
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=464
         return
      end if
C
C
      if (bdeb)write(*,*)myname,'Done.',irc
      return
C
      end subroutine reportSunPos
      subroutine reportSunRA(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*16 myname
      data myname /'reportSunRA'/
      external getSunRA
      real targetCycle, getSunRA
      real dtin                 ! initial time increment
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
C
      dtin=30.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getSunRA,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportSunRA
      subroutine reportMoonRA(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Moon celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*16 myname
      data myname /'reportMoonRA'/
      external getMoonRA
      real targetCycle, getMoonRA
      real dtin                 ! initial time increment
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
C
      dtin=3.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getMoonRA,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportMoonRA
      subroutine reportSunUpperElevation(Target,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dtin                   ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer pitch             ! 1: rising, -1: falling, 0:any
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*26 myname
      data myname /'reportSunUpperElevation'/
      external getSunUpperElevation
      real targetCycle,getSunUpperElevation
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
C
      call chop0(event100,100)
C
      dtin=0.02D0
      call reportLLTarget(Target,
     &     TargetCycle,getSunUpperElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportSunUpperElevation
      subroutine reportTarget(tval,
     &     tValCycle,funk,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      implicit none
      real tVal
      real tValCycle
      real funk
      external funk
      real tstartJD
      real tendJD
      real dtin
      integer secdec
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep
      integer nrep
      real repJD(max(1,maxrep))
      integer repid(max(1,maxrep))
      real repval(max(1,maxrep))
      character*250 rep250(max(1,maxrep))
      integer pitch             ! 1: rising, -1: falling, 0:any
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      character*12 myname
      data myname/'reportTarget'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      real ndt
      integer length,lend,lent
      externale length
      real diffcycle
      external diffcycle
      logical bdeb
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      JD=tstartJD
      mindt=(10.0D0**(-real(secdec)))/(3.33D0*86400D0)
      ndt=nint(0.50001D0+abs(tendJD-tstartJD)/max(1.0D-5,abs(dtin)))
      dt=abs(tendJD-tstartJD)/ndt
      dt=sign(max(6.9D-3,abs(dt)),tendJD-tstartJD)
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,mindt,dtin
      first=.true.
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     ((dt.gt.0.0D0.and.JD.le.tendJD+dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+dt)))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C
         if (bdeb) write(*,'(4(X,A),4(X,F16.8))')
     &        myname,'Step:',T16,event100(1:lend),val,tval,
     &        dval(1),dval(2)
C         write(*,*)myname,'JNK:',xval(1),xval(2),val
C
C     set boundaries (max/min)
C
         if (first.or.major) then
            xJD(1)=xJD(2)
            xJD(2)=JD
            xval(1)=xval(2)
            xval(2)=val
         end if
C
         dval(1)=diffCycle(xval(1),tval,tValCycle)
         dval(2)=diffCycle(xval(2),tval,tValCycle)
         dval(3)=diffCycle(val,tval,tValCycle)
C     
C     determine if we are in major/minor loop, and increment time
C
         if (first) then
            first=.false.
            idt=dt
            major=.true.        ! we always start in major loop
         else if (major) then   ! check if we must switch to minor loop
            major= (pitch.eq.-1.and.dval(2).gt.0.0D0).or.
     &           (pitch.eq.1.and.dval(1).gt.0.0D0) .or.
     &           ((dval(1).gt.0.0D0 .and. dval(2).gt.0.0D0).or.
     &           (dval(1).lt.0.0D0 .and. dval(2).lt.0.0D0)).or.
     &           (abs(dval(1)-dval(2)).gt.tValCycle*0.75D0)

            if (.not.major) then ! check if we have a cycle change
            end if
            if (.not.major) then ! we have just switched to minor loop
               if (bdeb)write(*,*)myname,'Entered minor loop.',dval(1),
     &              dval(2),dval(3),idt,xval(1),xval(2),val,tval
               xJD(3)=xJD(2)
               xval(3)=xval(2)
               idt= -0.5D0 * (xJD(2)-xJD(1))


            end if
         else if (abs(idt).lt.mindt) then ! we have completed the minor loop
            if (abs(dval(1)-dval(2)).lt.tValCycle*0.25D0 ! check if discontinuity-hit
     &           .or.tValCycle.lt.0.0D0) then ! check if discontinuity-hit
C     make report
               if ( (dt.gt.0.0D0.and.
     &              JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0D0.and.
     &              JD.ge.tendJD.and.
     &              JD.le.tstartJD) ) then
                  nrep=nrep+1
                  if (nrep.le.maxrep) then
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*
     &                       fact))/fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A)')T30(1:lent),
     &                    event100(1:lend)
                     repJD(nrep)=JD
                     repid(nrep)=eventId
                     repval(nrep)=-99999.0D0
                     if (bdeb)write(*,*)myname,'Ended minor loop.',idt
                  end if
               end if
            else if (bdeb) then
               write(*,*)myname,'False hit:',dval(1),dval(2),tValCycle
            end if
            
C     reset variables and continue search
            major=.true.
            xJD(2)=xJD(3)
            xval(2)=xval(3)
            JD=xJD(3)
            idt=dt
         else                   ! we are in minor loop
C            write(*,*)myname,'Minor before:',dval(1),dval(2),dval(3),idt
            if ((dval(3).gt.0.0D0.and.dval(1).gt.0.0D0).or.
     &           (dval(3).lt.0.0D0.and.dval(1).lt.0.0D0)) then
               xJD(1)=xJD(2) ! swap
               xval(1)=xval(2)
               dval(1)=dval(2)
            end if
            xJD(2)=JD
            xval(2)=val
            dval(2)=dval(3)
            idt= -0.5D0 * (xJD(2)-xJD(1)) ! go back half-a-step
C            write(*,*)myname,'Minor  after:',dval(1),dval(2),dval(3),idt
         end if
C     increment time
         JD=JD+idt
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.and.maxrep.ne.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=465
         return
      end if
C
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end subroutine reportTarget
C
      subroutine reportUmbra(
     &     tstartJDin,tendJDin,secdec,
     &     loccMax,occMax,loccMin,occMin,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJDin         ! start time
      real tendJDin           ! end time
      integer secdec            ! number of second-decimals
      logical loccMax
      real occMax
      logical loccMin
      real occMin
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repId(max(1,maxrep))     ! output report identification (SEE TABLE ABOVE)
      real repVal(max(1,maxrep))       ! output report value (SEE TABLE ABOVE)
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C     
      character*18 myname
      data myname /'reportUmbra'/
      external getMoonPhase,getUmbra,getPenUmbra,getUmbraOcc,getMoonOcc
      real getMoonPhase,getUmbra,getPenUmbra,getUmbraOcc,getMoonOcc
      real targetCycle
      parameter (targetCycle=100.0D0) ! target value which correponds to 0.0D0
      real dtin                 ! initial time increment
      real Rm,Rp,Ru,xCycle,xJD,pJD,dt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer xrep,maxxrep,yy1,mm1,dd1,hh1,mi1
      real sec1
      character*100 xevent100
      integer xeventId
      integer length,lend,leno,lent
      external length
      character*16 t16
      character*30 t30
      character*100 fmt100
      logical bdone
      real occ,fact
      logical proceed
      integer cnt,cntmax
      parameter (cntmax=10000)
C     
      call chop0(event100,100)
      lend=length(event100,100,10)
C
      Rm =  1738.0D0            ! moon radius
      bdone=.false.
      Target=50.0D0             ! Umbra entered at full moon
      tstartJD=tstartJDin
      dtin=sign(min(33.0D0,abs(tendJDin-tstartJDin)),
     &     tendJDin-tstartJDin)
      tendJD=tstartJDin + dtin
      cnt=0
      do while (cnt.lt.cntmax.and..not.bdone)
         cnt=cnt+1
         dt=dtin/10.0D0
         tendJD=tstartJD + dtin
         maxxrep=0
         xrep=0
         tstartJD=searchTarget(Target, ! only get time of 
     &        TargetCycle,getMoonPhase,
     &        tstartJD,tendJD,dt,secdec,
     &        eventId,event100,maxxrep,xrep,repJD,repid,repval,rep250,
     &        0,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from searchTarget.'
            call chop0(crc250,250)
            return
         end if
         if (xrep.gt.0) then    ! found a match, search backwards for Umbra entry
C            CALL JD2DATE(tstartJD,
C     &           YY1,
C     &           MM1,
C     &           DD1,
C     &           HH1,
C     &           MI1,
C     &           SEC1) 
C            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
C            write(*,*)myname,'Full moon:',t16
            maxxrep=0
            xrep=0
            tstartJD=searchMin( ! only get time of 
     &           getPenumbra,
     &           tstartJD-0.5D0,tstartJD+0.5D0,0.01D0,secdec,
     &           eventId,event100,
     &           maxxrep,xrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from searchMax.'
               call chop0(crc250,250)
               return
            end if
            Rp=getPenumbra(tstartJD,crc250,irc) ! distance between Moon center and earth-penumbra
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from GetPenumbra.'
               call chop0(crc250,250)
               return
            end if
            proceed=(Rp.lt.Rm)
            CALL JD2DATE(tstartJD,
     &           YY1,
     &           MM1,
     &           DD1,
     &           HH1,
     &           MI1,
     &           SEC1) 
            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888        FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C     write(*,*)myname,'Umbra:',t16, rp,rm,proceed
            if (proceed) then
               Ru=getUmbra(tstartJD,crc250,irc) ! distance between Moon center and earth-penumbra
               occ=getUmbraOcc()
               proceed=(proceed.and.(.not.loccMin.or.occ.ge.occMin))
               proceed=(proceed.and.(.not.loccMax.or.occ.le.occMax))
            end if
            if (proceed) then  ! we have a lunar eclipse
C     search backwards for Rp .eq. Rm (PENUMBRA PARTIAL START)
               dt=-0.2D0        ! eclipse starts not more than 1 day earlier
               xeventId=eventId + 0
               xevent100=event100(1:lend)//
     &              ' PENUMBRA CONTACT STARTS (P1)'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=max(min(tstartJDin,tendJDin),
     &              tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getPenumbra,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
               
C     write(*,*) myname,'Ru:',rp,rm,rp.le.rm
               if (Ru.le.Rm) then               
C     search backwards for Ru .eq. Rm (UMBRA PARTIAL START)
                  dt=-0.2D0     ! eclipse starts not more than 1 day earlier
                  xeventId=eventId + 1
                  xevent100=event100(1:lend)//
     &                 ' UMBRA CONTACT STARTS (U1)'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=max(min(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchTarget(Rm, ! find start
     &                 xCycle,getUmbra,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
               if (Ru.lt.-Rm) then ! Moon totally within Earth umbra
C     search backwards for Ru .eq. -Rm (UMBRA TOTAL START)
                  dt=-1.0D0     ! eclipse starts not more than 1 day earlier
                  xeventId=eventId + 2
                  xevent100=event100(1:lend)//
     &                 ' TOTAL ECLIPSE STARTS (U2)'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=max(min(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchTarget(-Rm, ! find start
     &                 xCycle,getUmbra,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
C     report max occulation
               if (Ru.le.Rm) then               
                  nrep=nrep+1
                  if (nrep.le.maxrep) then
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*
     &                       fact))/fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     lend=length(event100,100,30)
                     write(rep250(nrep),
     &                    '(A,X,A," MAX OCCULTATION=",F8.1,"%")')
     &                    T30(1:lent),event100(1:lend),
     &                    occ
                     repJD(nrep)=tstartJD
                     repid(nrep)=eventId + 3
                     repval(nrep)=occ
                  end if
               end if
               if (Ru.lt.-Rm) then ! Moon totally within Earth umbra
C     search forwards for Ru .eq. -Rm (UMBRA TOTAL STOP)
                  dt=0.2D0      ! eclipse stops not more than 1 day later
                  xeventId=eventId + 4
                  xevent100=event100(1:lend)//
     &                 ' TOTAL ECLIPSE STOPS (U3)'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=min(max(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchTarget(-Rm, ! find stop
     &                 xCycle,getUmbra,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
C     search forwards for Ru .eq. Rm (UMBRA PARTIAL STOP)
               if (Ru.lt.Rm) then
                  dt=1.0D0      ! eclipse stops not more than 1 day later
                  xeventId=eventId + 5
                  xevent100=event100(1:lend)//
     &                 ' UMBRA CONTACT STOPS (U4)'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=min(max(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchTarget(Rm, ! find stop
     &                 xCycle,getUmbra,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchTarget.'
                     call chop0(crc250,250)
                     return
                  end if
C     write(*,*) 'REPORTMP:',t16,xJD,nrep,rm
               end if
C     search forwards for Rp .eq. -Rm (PENUMBRA TOTAL STOP)
               dt=0.2D0         ! eclipse stops not more than 1 day later
               xeventId=eventId + 6
               xevent100=event100(1:lend)//
     &              ' PENUMBRA CONTACT STOPS (P2)'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=min(max(tstartJDin,tendJDin),
     &              tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find stop
     &              xCycle,getPenumbra,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         if ((dtin.gt.0.0D0.and.tstartJD+0.5*dtin
     &        .gt.tendJDin).or.
     &        (dtin.lt.0.0D0.and.tstartJD+0.5*dtin
     &        .lt.tendJDin)) then
            bdone=.true.
         else
            tstartJD=tstartJD+dtin*0.5D0 ! skip fast forward
         end if
      end do
C     
      if (cnt.ge.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
C     
      return
      end subroutine reportUmbra
      
      subroutine reportVenusRelLon(Target,
     &     tstartJD,tendJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJD           ! start time
      real tendJD             ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'reportVenusRelLon'/
      external getVenusRelLon
      real getVenusRelLon
      real targetCycle
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
C
      dtin=60.0D0
      call chop0(event100,100)
C
      call reportTarget(Target,
     &     TargetCycle,getVenusRelLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,
     &     maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from reportTarget.'
         call chop0(crc250,250)
         return
      end if
C
C
      return
      end subroutine reportVenusRelLon
      subroutine reportVenusTransit(
     &     tstartJDin,tendJDin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJDin         ! start time
      real tendJDin           ! end time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'reportVenusTransit'/
      external getVenusRelLon,getVenusTransit
      real targetCycle,getVenusRelLon,getVenusTransit
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
      real Rm,Rp,xCycle,xJD,pJD,dt
      real tstartJD         ! start time
      real tendJD           ! end time
      integer xrep,maxxrep,yy1,mm1,dd1,hh1,mi1
      real sec1
      character*100 xevent100
      integer xeventId
      integer length,lend
      external length
      character*16 t16
      logical bdone
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      call chop0(event100,100)
C
      Rm =   6051.0D0           ! venus radius
      bdone=.false.
      Target=0.0D0              ! Penumbra entered at venus inferior conjunction
      tstartJD=tstartJDin
      dtin=sign(min(600.0D0,abs(tendJDin-tstartJDin)),
     &     tendJDin-tstartJDin)
      tendJD=tstartJDin + dtin
      cnt=0
      do while (cnt.lt.cntmax.and..not.bdone)
         cnt=cnt+1
C
         dt=dtin/10.0D0
         tendJD=tstartJD + dtin

         maxxrep=0
         xrep=0
         tstartJD=searchTarget(Target, ! only get time of 
     &        TargetCycle,getVenusRelLon,
     &        tstartJD,tendJD,dt,secdec,
     &        eventId,event100,maxxrep,xrep,repJD,repid,repval,rep250,
     &        0,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from searchTarget.'
            call chop0(crc250,250)
            return
         end if

         if (xrep.gt.0) then    ! found a match, search backwards for penumbra entry

            CALL JD2DATE(tstartJD,
     &           YY1,
     &           MM1,
     &           DD1,
     &           HH1,
     &           MI1,
     &           SEC1) 
            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888        FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16

            Rp=getVenusTransit(tstartJD,crc250,irc) ! distance between Venus and earth-sun cone

C            write(*,*) myname,'Rp:',rp,rm,rp.le.rm
            if (Rp.le.Rm) then  ! Venus eclipses the Sun
               lend=length(event100,100,10)
               dt=10.0D0       ! eclipse starts not more than 10 day earlier
               xeventId=eventId + 1
               xevent100=event100(1:lend)//' STOP'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=min(max(tstartJDin,tendJDin),tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getVenusTransit,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
               dt=-10.0D0       ! eclipse starts not more than 10 day earlier
               xeventId=eventId + 0
               xevent100=event100(1:lend)//' START'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=max(min(tstartJDin,tendJDin),tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getVenusTransit,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
C               write(*,*) 'REPORTMP:',t16,xJD,nrep,rm
            end if
         end if
         if ((dtin.gt.0.0D0.and.tstartJD+0.5*dtin
     &        .gt.tendJDin).or.
     &        (dtin.lt.0.0D0.and.tstartJD+0.5*dtin
     &        .lt.tendJDin)) then
            bdone=.true.
         else
            tstartJD=tstartJD+dtin*0.5D0 ! skip fast forward
         end if
      end do
C
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
C
      return
      end subroutine reportVenusTransit
      real function searchJupiterLon(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'searchJupiterLon'/
      external getJupiterLon
      real targetCycle,getJupiterLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=40.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 450.0D0
      else
         tendJD = tstartJD - dtin - 450.0D0
      end if
C
      call chop0(event100,100)
C
      searchJupiterLon=searchTarget(Target,
     &     TargetCycle,getJupiterLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchJupiterLon

      real function searchLLMax(
     &     funk,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real funk                 ! function giving the value
      external funk             
      real lat                  ! observer latitude (deg)
      real lon                  ! observer longtitude (deg)
      real hgt                  ! observer height (not used)
      real tstartJD           ! start time in Julian day JD
      real tendJD             ! end time in Julian day JD
      real dtin                 ! time interval (absolute value)
      integer secdec            ! number of decimals in seconds output
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! maximum number of reports
      integer nrep              ! number of reports so far
      real repJD(max(1,maxrep)) ! report time in Julian day JD
      integer repid(max(1,maxrep)) ! report time in Julian day JD
      real repval(max(1,maxrep)) ! report time in Julian day JD
      character*250 rep250(max(1,maxrep)) ! report strings
      character*250 crc250      ! error description
      integer irc               ! error return code (0: ok)
      integer lencrc
C
      character*12 myname
      data myname/'searchLLMax'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,fact,dt
      real JD,val,val1,maxval,tol
      parameter (tol=1.0D-6)
      real xJD(3),xval(3),xa,xb,xc
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent,npts
      externale length
      logical bdeb,bdone
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      real vals,vale
      integer inrep
C     
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      JD=tstartJD-dt
      searchLLMax=tstartJD
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,dtin
      bdone=.false.
      inrep=nrep
      npts=0
      cnt=0
      do while (cnt.lt.cntmax.and.(.not.bdone.and.(
     &     (dt.gt.0.0D0.and.JD.le.tendJD+2.0D0*dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+2.0D0*dt))))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C
         if (bdeb) write(*,'(X,A,A,A,F10.3)')
     &        myname,T16,'Funk:',val
C     need 3 points before we can check for maximum
         xJD(1)=xJD(2)
         xJD(2)=xJD(3)
         xJD(3)=JD
         xval(1)=xval(2)
         xval(2)=xval(3)
         xval(3)=val
         npts=npts+1
         if (npts.ge.3) then
            if (xval(2).gt.xval(1).and.xval(2).gt.xval(3)) then
               xa=xJD(1)
               xb=xJD(2)
               xc=xJD(3)
               maxval=goldenLLMax(xa,xb,xc,
     &              funk,tol,JD,lat,lon,hgt,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from GoldenLLMax.'
                  call chop0(crc250,250)
                  return
               end if
               if ((dt.gt.0.0.and.JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0.and.JD.ge.tendJD.and.
     &              JD.le.tstartJD)) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  nrep=nrep+1
                  if (nrep.le.maxrep) then ! make report
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*fact))
     &                       /fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A," (",F17.5,")")')
     &                    T30(1:lent),event100(1:lend),maxval
                     repJD(nrep)=JD
                     repid(nrep)=eventId
                     repval(nrep)=maxval
                     if (bdeb)write(*,*)myname,'Ended minor loop.'
                  end if
                  searchLLMax=JD
                  bdone=.true.
               end if
               JD=xJD(3)
            end if
         end if
         JD=JD+dt         !  increment time
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.eq.inrep) then ! no local minimum, check end points
         vals=funk(tstartJD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         vale=funk(tendJD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         if (vals.ge.vale) then
            JD=tstartJD
            maxval=vals
         else
            JD=tendJD
            maxval=vale
         end if
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         nrep=nrep+1
         if (nrep.le.maxrep) then ! make report
            if (secdec.gt.0) then
               write(fmt100,'(A,I2.2,A,I1,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2,F",
     &              secdec+1,".",secdec,")"
               fact=10.0D0**(real(secdec))
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1),real(int((sec1-int(sec1))*fact))
     &              /fact
            else
               write(fmt100,'(A,I2.2,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2)"
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1)
            end if
            call chop0(T30,30)
            lent=length(T30,30,30)
            write(rep250(nrep),'(A,X,A," (",F17.5,")")')
     &           T30(1:lent),event100(1:lend),maxval
            repJD(nrep)=JD
            repid(nrep)=eventId
            repval(nrep)=maxval
            if (bdeb)write(*,*)myname,'Ended minor loop.'
         end if
         searchLLMax=JD
      end if
      if (nrep.gt.maxrep.AND.MAXREP.NE.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=466
         return
      end if
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end function searchLLMax

      real FUNCTION goldenLLMax(ax,bx,cx,f,tol,xmax,
     &     lat,lon,hgt,crc250,irc)
      implicit none
      REAL golden,ax,bx,cx,tol,xmax,lat,lon,hgt,f,R,C
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      EXTERNAL f
      PARAMETER (R=.61803399,C=1.-R)
C     Given a function f, and given a bracketing triplet of abscissas ax, bx, cx (such that bx is
C     between ax and cx, and f(bx) is less than both f(ax) and f(cx)), this routine performs
C     a golden section search for the maximum, isolating it to a fractional precision of about
C     tol. The abscissa of the maximum is returned as xmax, and the maximum function value
C     is returned as golden, the returned function value.
C     Parameters: The golden ratios.
      REAL f1,f2,x0,x1,x2,x3
      integer cnt,cntmax
      parameter (cntmax=10000)
      character*14 myname
      data myname/'goldenmax'/
      x0=ax                     !  At any given time we will keep track of four points, x0,x1,x2,x3.
      x3=cx
      if(abs(cx-bx).gt.abs(bx-ax))then ! Make x0 to x1 the smaller segment,
         x1=bx
         x2=bx+C*(cx-bx)        ! and fill in the new point to be tried.
      else
         x2=bx
         x1=bx-C*(bx-ax)
      endif
      f1=-f(x1,lat,lon,hgt,crc250,irc) ! The initial function evaluations. Note that we never need to
      if (irc.ne.0) return
      f2=-f(x2,lat,lon,hgt,crc250,irc) !  evaluate the function at the original endpoints.
      if (irc.ne.0) return
      cnt=0
 1    do while(cnt.lt.cntmax.and.(
     &     abs(x3-x0).gt.tol))
         cnt=cnt+1
         if(f2.lt.f1)then       ! One possible outcome,
            x0=x1               ! its housekeeping,
            x1=x2
            x2=R*x1+C*x3
            f1=f2
            f2=-f(x2,lat,lon,hgt,crc250,irc) ! and a new function evaluation.
            if (irc.ne.0) return
         else                   ! The other outcome,
            x3=x2
            x2=x1
            x1=R*x2+C*x0
            f2=f1
            f1=-f(x1,lat,lon,hgt,crc250,irc) ! and its new function evaluation.
            if (irc.ne.0) return
         endif
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if(f1.lt.f2)then          ! We are done. Output the best of the two current values.
         goldenLLMax=-f1
         xmax=x1
      else
         goldenLLMax=-f2
         xmax=x2
      endif
      return
      END function goldenLLMax
C
      real function searchLLMin(
     &     funk,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real funk                 ! function giving the value
      external funk
      real lat                  ! observer latitude (deg)
      real lon                  ! observer longtitude (deg)
      real hgt                  ! observer height (not used)
      real tstartJD           ! start time in Julian day JD
      real tendJD             ! end time in Julian day JD
      real dtin                 ! time interval (absolute value)
      integer secdec            ! number of decimals in seconds output
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! maximum number of reports
      integer nrep              ! number of reports so far
      real repJD(max(1,maxrep)) ! report time in Julian day JD
      integer repid(max(1,maxrep)) ! report time in Julian day JD
      real repval(max(1,maxrep)) ! report time in Julian day JD
      character*250 rep250(max(1,maxrep)) ! report strings
      character*250 crc250      ! error description
      integer irc               ! error return code (0: ok)
      integer lencrc
C
      character*12 myname
      data myname/'searchLLMin'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,fact,dt
      real JD,val,val1,minval,tol
      parameter (tol=1.0D-6)
      real xJD(3),xval(3),xa,xb,xc
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent,npts
      externale length
      logical bdeb,bdone
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      real vals,vale
      integer inrep
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      JD=tstartJD-dt
      searchLLMin=tstartJD
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,dtin
      bdone=.false.
      inrep=nrep
      npts=0
      cnt=0
      do while (cnt.lt.cntmax.and.(.not.bdone.and.(
     &     (dt.gt.0.0D0.and.JD.le.tendJD+2.0D0*dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+2.0D0*dt))))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         if (bdeb) write(*,'(X,A,A,A,F10.3)')
     &        myname,T16,'Funk:',val
C     need 3 points before we can check for maximum
         xJD(1)=xJD(2)
         xJD(2)=xJD(3)
         xJD(3)=JD
         xval(1)=xval(2)
         xval(2)=xval(3)
         xval(3)=val
         npts=npts+1
         if (npts.ge.3) then
            if (xval(2).lt.xval(1).and.xval(2).lt.xval(3)) then
               xa=xJD(1)
               xb=xJD(2)
               xc=xJD(3)
               minval=goldenLLMin(xa,xb,xc,
     &              funk,tol,JD,lat,lon,hgt,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from GoldenLLMin.'
                  call chop0(crc250,250)
                  return
               end if
               if ((dt.gt.0.0.and.JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0.and.JD.ge.tendJD.and.
     &              JD.le.tstartJD)) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  nrep=nrep+1
                  if (nrep.le.maxrep) then ! make report
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*fact))
     &                       /fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A," (",F17.5,")")')
     &                    T30(1:lent),event100(1:lend),minval
                     repJD(nrep)=JD
                     repid(nrep)=eventId
                     repval(nrep)=minval
                     if (bdeb)write(*,*)myname,'Ended minor loop.'
                  end if
                  searchLLMin=JD
                  bdone=.true.
               end if
               JD=xJD(3)
            end if
         end if
         JD=JD+dt         !  increment time
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.eq.inrep) then ! no local minimum, check end points
         vals=funk(tstartJD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         vale=funk(tendJD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         if (vals.le.vale) then
            JD=tstartJD
            minval=vals
         else
            JD=tendJD
            minval=vale
         end if
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         nrep=nrep+1
         if (nrep.le.maxrep) then ! make report
            if (secdec.gt.0) then
               write(fmt100,'(A,I2.2,A,I1,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2,F",
     &              secdec+1,".",secdec,")"
               fact=10.0D0**(real(secdec))
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1),real(int((sec1-int(sec1))*fact))
     &              /fact
            else
               write(fmt100,'(A,I2.2,A)')
     &              "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &              "I2.2,':',I2.2)"
               write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &              int(sec1)
            end if
            call chop0(T30,30)
            lent=length(T30,30,30)
            write(rep250(nrep),'(A,X,A," (",F17.5,")")')
     &           T30(1:lent),event100(1:lend),minval
            repJD(nrep)=JD
            repid(nrep)=eventId
            repval(nrep)=minval
            if (bdeb)write(*,*)myname,'Ended minor loop.'
         end if
         searchLLMin=JD
      end if
      if (nrep.gt.maxrep.AND.MAXREP.NE.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=467
         return
      end if
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end function searchLLMin

      real FUNCTION goldenLLMin(ax,bx,cx,f,tol,xmin,
     &     lat,lon,hgt,crc250,irc)
      implicit none
      REAL golden,ax,bx,cx,tol,xmin,lat,lon,hgt,f,R,C
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      EXTERNAL f
      PARAMETER (R=.61803399,C=1.-R)
C     Given a function f, and given a bracketing triplet of abscissas ax, bx, cx (such that bx is
C     between ax and cx, and f(bx) is less than both f(ax) and f(cx)), this routine performs
C     a golden section search for the minimum, isolating it to a fractional precision of about
C     tol. The abscissa of the minimum is returned as xmin, and the minimum function value
C     is returned as golden, the returned function value.
C     Parameters: The golden ratios.
      REAL f1,f2,x0,x1,x2,x3
      integer cnt,cntmax
      parameter (cntmax=10000)
      character*14 myname
      data myname/'goldenLLmin'/
      x0=ax                     !  At any given time we will keep track of four points, x0,x1,x2,x3.
      x3=cx
      if(abs(cx-bx).gt.abs(bx-ax))then ! Make x0 to x1 the smaller segment,
         x1=bx
         x2=bx+C*(cx-bx)        ! and fill in the new point to be tried.
      else
         x2=bx
         x1=bx-C*(bx-ax)
      endif
      f1=f(x1,lat,lon,hgt,crc250,irc)  ! The initial function evaluations. Note that we never need to
      if (irc.ne.0) return
      f2=f(x2,lat,lon,hgt,crc250,irc)  !  evaluate the function at the original endpoints.
      if (irc.ne.0) return
      cnt=0
 1    do while(cnt.lt.cntmax.and.
     &     (abs(x3-x0).gt.tol))
         cnt=cnt+1
         if(f2.lt.f1)then       ! One possible outcome,
            x0=x1               ! its housekeeping,
            x1=x2
            x2=R*x1+C*x3
            f1=f2
            f2=f(x2,lat,lon,hgt,crc250,irc) ! and a new function evaluation.
            if (irc.ne.0) return
         else                   ! The other outcome,
            x3=x2
            x2=x1
            x1=R*x2+C*x0
            f2=f1
            f1=f(x1,lat,lon,hgt,crc250,irc) ! and its new function evaluation.
            if (irc.ne.0) return
         endif
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if(f1.lt.f2)then          ! We are done. Output the best of the two current values.
         goldenLLMin=f1
         xmin=x1
      else
         goldenLLMin=f2
         xmin=x2
      endif
      return
      END function goldenLLMin
C
      real function searchLLTarget(tval,
     &     tValCycle,funk,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      implicit none
      real tVal                 ! target value
      real tValCycle            ! target value cycle (360.0D0 for degrees)
      real funk                 ! function giving the value
      external funk
      real lat                  ! observer latitude (deg)
      real lon                  ! observer longtitude (deg)
      real hgt                  ! observer height (not used)
      real tstartJD           ! start time in Julian day JD
      real tendJD             ! end time in Julian day JD
      real dtin                 ! time interval (absolute value)
      integer secdec            ! number of decimals in seconds output
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! maximum number of reports
      integer nrep              ! number of reports so far
      real repJD(max(1,maxrep)) ! report time in Julian day JD
      integer repid(max(1,maxrep)) ! report time in Julian day JD
      real repval(max(1,maxrep)) ! report time in Julian day JD
      character*250 rep250(max(1,maxrep)) ! report strings
      integer pitch             ! 1: rising, -1: falling, 0:any
      character*250 crc250      ! error description
      integer irc               ! error return code (0: ok)
      integer lencrc
C     
      character*14 myname
      data myname/'searchLLTarget'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      real ndt
      integer length,lend,lent
      externale length
      real diffcycle
      external diffcycle
      logical bdeb,bdone
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      JD=tstartJD
      searchLLTarget=tstartJD
      mindt=(10.0D0**(-real(secdec)))/(3.33D0*86400D0)
      ndt=nint(0.50001D0+abs(tendJD-tstartJD)/max(1.0D-5,abs(dtin)))
      dt=abs(tendJD-tstartJD)/ndt
      dt=sign(max(6.9D-3,abs(dt)),tendJD-tstartJD)
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,mindt,dtin
      searchLLTarget=tendJD
      first=.true.
      bdone=.false.
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     (.not.bdone.and.(
     &     (dt.gt.0.0D0.and.JD.le.tendJD).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD))))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,lat,lon,hgt,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C
         if (bdeb)write(*,'(4(X,A),2(X,F20.8))')
     &        myname,'Step:',T16,event100(1:lend),val,tval
C         write(*,*)myname,'JNK:',xval(1),xval(2),val,val1
C
C     set boundaries (max/min)
C
         if (first.or.major) then
            xJD(1)=xJD(2)
            xJD(2)=JD
            xval(1)=xval(2)
            xval(2)=val
         end if
C
         dval(1)=diffCycle(xval(1),tval,tValCycle)
         dval(2)=diffCycle(xval(2),tval,tValCycle)
         dval(3)=diffCycle(val,tval,tValCycle)
C     
C     determine if we are in major/minor loop, and increment time
C
         if (first) then
            first=.false.
            idt=dt
            major=.true.        ! we always start in major loop
         else if (major) then   ! check if we must switch to minor loop
            major= (pitch.eq.-1.and.dval(2).gt.0.0D0).or.
     &           (pitch.eq.1.and.dval(1).gt.0.0D0) .or.
     &           ((dval(1).gt.0.0D0 .and. dval(2).gt.0.0D0).or.
     &           (dval(1).lt.0.0D0 .and. dval(2).lt.0.0D0)).or.
     &           (tvalcycle.gt.0.0D0.and.
     &           abs(dval(1)-dval(2)).gt.tValCycle*0.75D0)

            if (.not.major) then ! check if we have a cycle change
            end if
            if (.not.major) then ! we have just switched to minor loop
               if (bdeb)write(*,*)myname,'Entered minor loop.',dval(1),
     &              dval(2),dval(3),idt,xval(1),xval(2),val,tval
               xJD(3)=xJD(2)
               xval(3)=xval(2)
               idt= -0.5D0 * (xJD(2)-xJD(1))


            end if
         else if (abs(idt).lt.mindt) then ! we have completed the minor loop
            if (abs(dval(1)-dval(2)).lt.tValCycle*0.25D0 ! check if discontinuity-hit
     &           .or.tValCycle.lt.0.0D0) then
               nrep=nrep+1
               if (maxrep.ne.0) then ! make report
                  if (nrep.le.maxrep) then
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))
     &                       *fact))/fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A)')T30(1:lent),
     &                    event100(1:lend)
                     repJD(nrep)=JD
                     repid(nrep)=eventId
                     repval(nrep)=-99999.0D0
                     if (bdeb)write(*,*)myname,'Ended minor loop.',idt
                  end if
               end if
               searchLLTarget=JD
               bdone=.true.
            end if
C     reset variables
            major=.true.
            xJD(2)=xJD(3)
            xval(2)=xval(3)
            JD=xJD(3)
            idt=dt
         else                   ! we are in minor loop
C            write(*,*)myname,'Minor before:',dval(1),dval(2),dval(3),idt
            if ((dval(3).gt.0.0D0.and.dval(1).gt.0.0D0).or.
     &           (dval(3).lt.0.0D0.and.dval(1).lt.0.0D0)) then
               xJD(1)=xJD(2) ! swap
               xval(1)=xval(2)
               dval(1)=dval(2)
            end if
            xJD(2)=JD
            xval(2)=val
            dval(2)=dval(3)
            idt= -0.5D0 * (xJD(2)-xJD(1)) ! go back half-a-step
C            write(*,*)myname,'Minor  after:',dval(1),dval(2),dval(3),idt
         end if
C     increment time
         JD=JD+idt
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.AND.MAXREP.NE.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=468
         return
      end if
C
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end function searchLLTarget
C
      real function searchLunarTime(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchLunarTime'/
      external getLunarTime
      real targetCycle,getLunarTime
      parameter (targetCycle=24.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=0.1D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 1.5D0
      else
         tendJD = tstartJD - dtin - 1.5D0
      end if
C
      call chop0(event100,100)
C
      searchLunarTime=searchLLTarget(Target,
     &     TargetCycle,getLunarTime,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchLunarTime

      SUBROUTINE searchLunarTimeHardy(f_time,
     & target,
     & lat, lon, height,
     & tstartJD,
     & secdec,
     & direction,
     & irc)
      implicit none

      REAL, INTENT(OUT) :: f_time
      REAL, INTENT(IN) :: target, lat, lon, height
      REAL, INTENT(IN) :: tstartJD
      INTEGER, INTENT(IN) :: secdec
      INTEGER, INTENT(IN) :: direction
      INTEGER, INTENT(INOUT) :: irc


      INTEGER  :: maxrep
      data maxrep /1/
      INTEGER  :: nrep 
      REAL     :: repJD(1)
      CHARACTER*250 :: CRC250
      CHARACTER*250 :: rep250(1)
      CHARACTER*100 :: event100
      INTEGER :: I
      REAL    :: searchLunarTime
      EXTERNAL searchLunarTime

      nrep = 0

      f_time =  searchLunarTime(target,
     &                       lat, lon, height, tstartJD,
     &                       secdec,
     &                       maxrep,
     &                       nrep,
     &                       repJD,
     &                       rep250,
     &                       event100,
     &                       direction,
     &                       crc250,irc)

      END SUBROUTINE searchLunarTimeHardy
      real function searchMarsLon(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'searchMarsLon'/
      external getMarsLon
      real targetCycle,getMarsLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=80.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 800.0D0
      else
         tendJD = tstartJD - dtin - 800.0D0
      end if
C
      call chop0(event100,100)
C
      searchMarsLon=searchTarget(Target,
     &     TargetCycle,getMarsLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMarsLon

      real function searchMax(
     &     funk,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real funk ! function giving the value
      external funk
      real tstartJD  ! start time in Julian day JD
      real tendJD  ! end time in Julian day JD
      real dtin   ! time interval (absolute value)
      integer secdec  ! number of decimals in seconds output
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep  ! maximum number of reports
      integer nrep  ! number of reports so far
      real repJD(max(1,maxrep)) ! report time in Julian day JD
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! report strings
      character*250 crc250      ! error description
      integer irc ! error return code (0: ok)
      integer lencrc
C
      character*12 myname
      data myname/'searchMax'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,fact,dt
      real JD,val,val1,maxval,tol
      parameter (tol=1.0D-6)
      real xJD(3),xval(3),xa,xb,xc
      real tmax,vmax
      logical first,found
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent,npts
      externale length
      logical bdeb,bdone
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      JD=tstartJD-dt
      searchMax=tstartJD
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,dtin
      bdone=.false.
      first=.true.
      found=.false.
      npts=0
      cnt=0
      do while (cnt.lt.cntmax.and.
     &     (.not.bdone.and.(
     &     (dt.gt.0.0D0.and.JD.le.tendJD+2.0D0*dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+2.0D0*dt))))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         if (first) then
            first=.false.
            tmax=JD
            vmax=val
         else if (val.gt.vmax) then
            vmax=val
            tmax=JD
         end if
         if (bdeb) write(*,'(X,A,A,A,F10.3)')
     &        myname,T16,'Funk:',val
C     need 3 points before we can check for maximum
         xJD(1)=xJD(2)
         xJD(2)=xJD(3)
         xJD(3)=JD
         xval(1)=xval(2)
         xval(2)=xval(3)
         xval(3)=val
         npts=npts+1
         if (npts.ge.3) then
            if (xval(2).gt.xval(1).and.xval(2).gt.xval(3)) then
               xa=xJD(1)
               xb=xJD(2)
               xc=xJD(3)
               maxval=goldenMax(xa,xb,xc,
     &              funk,tol,JD,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from GoldenMax.'
                  call chop0(crc250,250)
                  return
               end if
               if ((dt.gt.0.0.and.JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0.and.JD.ge.tendJD.and.
     &              JD.le.tstartJD)) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  nrep=nrep+1
                  if (nrep.le.maxrep) then ! make report
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*fact))
     &                       /fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A," (",F17.5,")")')
     &                    T30(1:lent),event100(1:lend),maxval
                     repJD(nrep)=JD
                     repid(nrep)=eventId
                     repval(nrep)=maxval
                     if (bdeb)write(*,*)myname,'Ended minor loop.',
     *                    JD,bdone
                  end if
                  searchMax=JD
                  bdone=.true.
                  found=.true.
               end if
               JD=xJD(3)
            end if
         end if
         JD=JD+dt         !  increment time
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.AND.MAXREP.NE.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=469
         return
      end if
C
      if (bdeb) write(*,*)myname,'Found:',found,tmax,vmax
      if (.not.found) then
         searchMax=tmax
      end if
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end function searchMax
      real FUNCTION goldenMax(ax,bx,cx,f,tol,xmax,crc250,irc)
      implicit none
      REAL golden,ax,bx,cx,tol,xmax,f,R,C
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      EXTERNAL f
      PARAMETER (R=.61803399,C=1.-R)
C     Given a function f, and given a bracketing triplet of abscissas ax, bx, cx (such that bx is
C     between ax and cx, and f(bx) is less than both f(ax) and f(cx)), this routine performs
C     a golden section search for the maximum, isolating it to a fractional precision of about
C     tol. The abscissa of the maximum is returned as xmax, and the maximum function value
C     is returned as golden, the returned function value.
C     Parameters: The golden ratios.
      REAL f1,f2,x0,x1,x2,x3
      integer cnt,cntmax
      parameter (cntmax=10000)
      character*14 myname
      data myname/'goldenmax'/
      x0=ax                     !  At any given time we will keep track of four points, x0,x1,x2,x3.
      x3=cx
      if(abs(cx-bx).gt.abs(bx-ax))then ! Make x0 to x1 the smaller segment,
         x1=bx
         x2=bx+C*(cx-bx)        ! and fill in the new point to be tried.
      else
         x2=bx
         x1=bx-C*(bx-ax)
      endif
      f1=-f(x1,crc250,irc)                  ! The initial function evaluations. Note that we never need to
      if (irc.ne.0) return
      f2=-f(x2,crc250,irc)                  !  evaluate the function at the original endpoints.
      if (irc.ne.0) return
      cnt=0
 1    do while(cnt.lt.cntmax.and.
     &     (abs(x3-x0).gt.tol))
         cnt=cnt+1
         if(f2.lt.f1)then       ! One possible outcome,
            x0=x1               ! its housekeeping,
            x1=x2
            x2=R*x1+C*x3
            f1=f2
            f2=-f(x2,crc250,irc)            ! and a new function evaluation.
            if (irc.ne.0) return
         else                   ! The other outcome,
            x3=x2
            x2=x1
            x1=R*x2+C*x0
            f2=f1
            f1=-f(x1,crc250,irc)            ! and its new function evaluation.
            if (irc.ne.0) return
         endif
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if(f1.lt.f2)then          ! We are done. Output the best of the two current values.
         goldenMax=-f1
         xmax=x1
      else
         goldenMax=-f2
         xmax=x2
      endif
      return
      END function goldenMax
      real function searchMaxMercElong(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMaxMercElong'/
      external getMercElong
      real getMercElong
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=15.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 150.0D0
      else
         tendJD = tstartJD - dtin - 150.0D0
      end if
C
      call chop0(event100,100)
C
      searchMaxMercElong=searchMax(
     &     getMercElong,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMaxMercElong

      real function searchMaxMoonElevation(
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100    ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=k)
      integer lencrc
C
      character*22 myname
      data myname /'searchMaxMoonElevation'/
      external getMoonElevation
      real getMoonElevation
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=0.02D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 1.5D0
      else
         tendJD = tstartJD - dtin - 1.5D0
      end if
C
      call chop0(event100,100)
C
      searchMaxMoonElevation=searchLLMax(
     &     getMoonElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMaxMoonElevation

      real function searchMaxMoonIllum(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMaxMoonIllum'/
      external getMoonIllum
      real getMoonIllum
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=3.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 30.0D0
      else
         tendJD = tstartJD - dtin - 30.0D0
      end if
C
      call chop0(event100,100)
C
      searchMaxMoonIllum=searchMax(
     &     getMoonIllum,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMaxMoonIllum

      real function searchMaxMoonEarthDist(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMaxMoonEarthDist'/
      external getMoonEarthDist
      real getMoonEarthDist
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=3.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 30.0D0
      else
         tendJD = tstartJD - dtin - 30.0D0
      end if
C
      call chop0(event100,100)
C
      searchMaxMoonEarthDist=searchMax(
     &     getMoonEarthDist,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMaxMoonEarthDist

      real function searchMaxSunEarthDist(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMaxSunEarthDist'/
      external getSunEarthDist
      real getSunEarthDist
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=30.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 370.0D0
      else
         tendJD = tstartJD - dtin - 370.0D0
      end if
C
      call chop0(event100,100)
C
      searchMaxSunEarthDist=searchMax(
     &     getSunEarthDist,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMaxSunEarthDist

      real function searchMaxSunElevation(
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMaxSunElevation'/
      external getSunElevation
      real getSunElevation
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      call chop0(event100,100)
C
      dtin=0.02D0                  ! initial time step
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 1.0D0
      else
         tendJD = tstartJD - dtin - 1.0D0
      end if
      searchMaxSunElevation=searchLLMax(
     &     getSunElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMaxSunElevation

      real function searchMaxVenusElong(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMaxVenusElong'/
      external getVenusElong
      real getVenusElong
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=60.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 600.0D0
      else
         tendJD = tstartJD - dtin - 600.0D0
      end if
C
      call chop0(event100,100)
C
      searchMaxVenusElong=searchMax(
     &     getVenusElong,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMax.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMaxVenusElong

      real function searchMercRelLon(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'searchMercRelLon'/
      external getMercRelLon
      real targetCycle,getMercRelLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=15.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 150.0D0
      else
         tendJD = tstartJD - dtin - 150.0D0
      end if
C
      call chop0(event100,100)
C
      searchMercRelLon=searchTarget(Target,
     &     TargetCycle,getMercRelLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMercRelLon

      real function searchMercTransit(
     &     tstartJDin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJDin         ! start time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! search direction
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchMercTransit'/
      external getMercRelLon,getMercTransit
      real targetCycle,getMercRelLon,getMercTransit
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
      real Rm,Rp,xCycle,xJD,pJD,dt
      real tstartJD         ! start time
      real tendJD           ! end time
      integer xrep,maxxrep,yy1,mm1,dd1,hh1,mi1
      real sec1
      character*100 xevent100
      integer xeventId
      integer length,lend
      external length
      character*16 t16
      real tendJDin           ! end time
      logical bdone
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      call chop0(event100,100)
C
      if (direction.eq.1) then
         tendJDin = tstartJDin + 50.0D0*365.0D0
      else
         tendJDin = tstartJDin - 50.0D0*365.0D0
      end if
C
      searchMercTransit=0.0D0
      Rm =   2440.0D0           ! mercury radius
      bdone=.false.
      Target=0.0D0 ! Penumbra entered at mercury inferior conjunction
      tstartJD=tstartJDin
      dtin=sign(min(150.0D0,abs(tendJDin-tstartJDin)),
     &     tendJDin-tstartJDin)
      tendJD=tstartJDin + dtin
      cnt=0
      do while (cnt.lt.cntmax.and..not.bdone)
         cnt=cnt+1
C     
         dt=dtin/10.0D0
         tendJD=tstartJD + dtin

         maxxrep=0
         xrep=0
         tstartJD=searchTarget(Target, ! only get time of 
     &        TargetCycle,getMercRelLon,
     &        tstartJD,tendJD,dt,secdec,
     &        eventId,event100,maxxrep,xrep,repJD,repid,repval,rep250,
     &        0,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from searchTarget.'
            call chop0(crc250,250)
            return
         end if

         if (xrep.gt.0) then    ! found a match, search backwards for penumbra entry

            CALL JD2DATE(tstartJD,
     &           YY1,
     &           MM1,
     &           DD1,
     &           HH1,
     &           MI1,
     &           SEC1) 
            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888        FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16

            Rp=getMercTransit(tstartJD,crc250,irc) ! distance between Merc and earth-sun cone

C           write(*,*) myname,'Rp:',T16,rp,rm,rp.le.rm
            if (Rp.le.Rm) then  ! Mercury eclipses the Sun
               lend=length(event100,100,10)
               xrep=nrep
               dt=10.0D0       ! eclipse starts not more than 10 day earlier
               xeventId=eventId + 1
               xevent100=event100(1:lend)//' STOP'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=min(max(tstartJDin,tendJDin),tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getMercTransit,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
               dt=-10.0D0       ! eclipse starts not more than 10 day earlier
               xeventId=eventId + 0
               xevent100=event100(1:lend)//' START'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=max(min(tstartJDin,tendJDin),tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getMercTransit,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
               if (nrep.ne.xrep) then ! we are done...
                  searchMercTransit=xJD
                  return
               end if
               return

C               write(*,*) 'REPORTMP:',t16,xJD,nrep,rm
            end if
         end if
         if ((dtin.gt.0.0D0.and.tstartJD+0.5*dtin
     &        .gt.tendJDin).or.
     &        (dtin.lt.0.0D0.and.tstartJD+0.5*dtin
     &        .lt.tendJDin)) then
            bdone=.true.
         else
            tstartJD=tstartJD+dtin*0.5D0 ! skip fast forward
         end if
      end do
C
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
C
      return
      end function searchMercTransit
      real function searchMin(
     &     funk,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      implicit none
      real funk                 ! function giving the value
      external funk
      real tstartJD           ! start time in Julian day JD
      real tendJD             ! end time in Julian day JD
      real dtin                 ! time interval (absolute value)
      integer secdec            ! number of decimals in seconds output
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! maximum number of reports
      integer nrep              ! number of reports so far
      real repJD(max(1,maxrep)) ! report time in Julian day JD
      integer repid(max(1,maxrep)) ! report time in Julian day JD
      real repval(max(1,maxrep)) ! report time in Julian day JD
      character*250 rep250(max(1,maxrep)) ! report strings
      character*250 crc250      ! error description
      integer irc               ! error return code (0: ok)
      integer lencrc
C
      character*12 myname
      data myname/'searchMin'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,fact,dt
      real JD,val,val1,minval,tol
      parameter (tol=1.0D-6)
      real xJD(3),xval(3),xa,xb,xc
      real tmin,vmin
      logical first,found
      character*100 fmt100
      character*30 T30
      character*16 t16
      integer length,lend,lent,npts
      externale length
      logical bdeb,bdone
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      dt=sign(max(6.9D-3,abs(dtin)),tendJD-tstartJD)
      JD=tstartJD-dt
      searchMin=tstartJD
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,dtin
      bdone=.false.
      first=.true.
      found=.false.
      npts=0
      cnt=0
      do while (cnt.lt.cntmax.and.(.not.bdone.and.(
     &     (dt.gt.0.0D0.and.JD.le.tendJD+2.0D0*dt).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD+2.0D0*dt))))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
         if (first) then
            first=.false.
            tmin=JD
            vmin=val
         else if (val.lt.vmin) then
            vmin=val
            tmin=JD
         end if
         if (bdeb) write(*,'(X,A,A,A,F10.3)')
     &        myname,T16,' Funk:',val
C     need 3 points before we can check for maximum
         xJD(1)=xJD(2)
         xJD(2)=xJD(3)
         xJD(3)=JD
         xval(1)=xval(2)
         xval(2)=xval(3)
         xval(3)=val
         npts=npts+1
         if (npts.ge.3) then
            if (xval(2).lt.xval(1).and.xval(2).lt.xval(3)) then
               if (bdeb)write(*,*)myname,'Started minor loop.'
               xa=xJD(1)
               xb=xJD(2)
               xc=xJD(3)
               minval=goldenMin(xa,xb,xc,
     &              funk,tol,JD,crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from GoldenMin.'
                  call chop0(crc250,250)
                  return
               end if
               if (bdeb) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
                  write(*,*)myname,'Minor loop done,',t16,minval,
     &                 dt,JD,tstartJD,tendJD
               end if
               if ((dt.gt.0.0.and.JD.le.tendJD.and.
     &              JD.ge.tstartJD).or.
     &              (dt.lt.0.0.and.JD.ge.tendJD.and.
     &              JD.le.tstartJD)) then
                  CALL JD2DATE(JD,
     &                 YY1,
     &                 MM1,
     &                 DD1,
     &                 HH1,
     &                 MI1,
     &                 SEC1) 
                  nrep=nrep+1
                  if (nrep.le.maxrep) then ! make report
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*fact))
     &                       /fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A," (",F17.5,")")')
     &                    T30(1:lent),event100(1:lend),minval
                     repJD(nrep)=JD
                     repid(nrep)=eventId
                     repval(nrep)=minval
                     if (bdeb)write(*,*)myname,'Ended minor loop.',
     *                    JD,bdone
                  end if
                  searchMin=JD
                  bdone=.true.
                  found=.true.
               end if
               JD=xJD(3)
            end if
         end if
         JD=JD+dt         !  increment time
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.AND.MAXREP.NE.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=470
         return
      end if
C
      if (.not.found) then
         searchMin=tmin
      end if
C     
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end function searchMin
C
      real FUNCTION goldenMin(ax,bx,cx,f,tol,xmin,crc250,irc)
      implicit none
      REAL golden,ax,bx,cx,tol,xmin,f,R,C
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      EXTERNAL f
      PARAMETER (R=.61803399,C=1.-R)
C     Given a function f, and given a bracketing triplet of abscissas ax, bx, cx (such that bx is
C     between ax and cx, and f(bx) is less than both f(ax) and f(cx)), this routine performs
C     a golden section search for the minimum, isolating it to a fractional precision of about
C     tol. The abscissa of the minimum is returned as xmin, and the minimum function value
C     is returned as golden, the returned function value.
C     Parameters: The golden ratios.
      REAL f1,f2,x0,x1,x2,x3
      integer cnt,cntmax
      parameter (cntmax=10000)
      character*14 myname
      data myname/'goldenmin'/
      x0=ax                     !  At any given time we will keep track of four points, x0,x1,x2,x3.
      x3=cx
      if(abs(cx-bx).gt.abs(bx-ax))then ! Make x0 to x1 the smaller segment,
         x1=bx
         x2=bx+C*(cx-bx)        ! and fill in the new point to be tried.
      else
         x2=bx
         x1=bx-C*(bx-ax)
      endif
      f1=f(x1,crc250,irc)                  ! The initial function evaluations. Note that we never need to
      if (irc.ne.0) return
      f2=f(x2,crc250,irc)                  !  evaluate the function at the original endpoints.
      if (irc.ne.0) return
      cnt=0
 1    do while(cnt.lt.cntmax.and.
     &     (abs(x3-x0).gt.tol))
         cnt=cnt+1
         if(f2.lt.f1)then       ! One possible outcome,
            x0=x1               ! its housekeeping,
            x1=x2
            x2=R*x1+C*x3
            f1=f2
            f2=f(x2,crc250,irc)            ! and a new function evaluation.
            if (irc.ne.0) return
         else                   ! The other outcome,
            x3=x2
            x2=x1
            x1=R*x2+C*x0
            f2=f1
            f1=f(x1,crc250,irc)            ! and its new function evaluation.
            if (irc.ne.0) return
         endif
C         write(*,*) 'GoldenMin:',f1,f2,x1,x2
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if(f1.lt.f2)then          ! We are done. Output the best of the two current values.
         goldenMin=f1
         xmin=x1
      else
         goldenMin=f2
         xmin=x2
      endif
      return
      END function goldenMin
C
      real function searchMinMercElong(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinMercElong'/
      external getMercElong
      real getMercElong
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=15.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 150.0D0
      else
         tendJD = tstartJD - dtin - 150.0D0
      end if
C
      call chop0(event100,100)
C
      searchMinMercElong=searchMin(
     &     getMercElong,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMinMercElong

      real function searchMinMoonElevation(
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinMoonElevation'/
      external getMoonElevation
      real getMoonElevation
C
      dtin=0.02D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 1.5D0
      else
         tendJD = tstartJD - dtin - 1.5D0
      end if
C
      call chop0(event100,100)
C
      searchMinMoonElevation=searchLLMin(
     &     getMoonElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMinMoonElevation

      real function searchMinMoonIllum(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinMoonIllum'/
      external getMoonIllum
      real getMoonIllum
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=3.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 30.0D0
      else
         tendJD = tstartJD - dtin - 30.0D0
      end if
C
      call chop0(event100,100)
C
      searchMinMoonIllum=searchMin(
     &     getMoonIllum,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMinMoonIllum

      real function searchMinMoonEarthDist(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinMoonEarthDist'/
      external getMoonEarthDist
      real getMoonEarthDist
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=3.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 30.0D0
      else
         tendJD = tstartJD - dtin - 30.0D0
      end if
C
      call chop0(event100,100)
C
      searchMinMoonEarthDist=searchMin(
     &     getMoonEarthDist,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMinMoonEarthDist

      real function searchMinSunEarthDist(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinSunEarthDist'/
      external getSunEarthDist
      real getSunEarthDist
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=30.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 370.0D0
      else
         tendJD = tstartJD - dtin - 370.0D0
      end if
C
      call chop0(event100,100)
C
      searchMinSunEarthDist=searchMin(
     &     getSunEarthDist,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMinSunEarthDist

      real function searchMinSunElevation(
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinSunElevation'/
      external getSunElevation
      real getSunElevation
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      call chop0(event100,100)
C
      dtin=0.02D0                  ! initial time step
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 1.0D0
      else
         tendJD = tstartJD - dtin - 1.0D0
      end if
      searchMinSunElevation=searchLLMin(
     &     getSunElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMinSunElevation

      real function searchMinVenusElong(
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         !  +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*22 myname
      data myname /'searchMinVenusElong'/
      external getVenusElong
      real getVenusElong
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=60.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 600.0D0
      else
         tendJD = tstartJD - dtin - 600.0D0
      end if
C
      call chop0(event100,100)
C
      searchMinVenusElong=searchMin(
     &     getVenusElong,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchMin.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMinVenusElong

      real function searchMoonAzimuth(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchMoonAzimuth'/
      external getMoonAzimuth
      real targetCycle,getMoonAzimuth
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=0.1D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 1.5D0
      else
         tendJD = tstartJD - dtin - 1.5D0
      end if
C
      call chop0(event100,100)
C
      searchMoonAzimuth=searchLLTarget(Target,
     &     TargetCycle,getMoonAzimuth,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMoonAzimuth

      real function searchMoonIllum(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*16 myname
      data myname /'searchMoonIllum'/
      external getMoonIllum
      real targetCycle,getMoonIllum
      parameter (targetCycle=100.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=3.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 30.0D0
      else
         tendJD = tstartJD - dtin - 30.0D0
      end if
C
      call chop0(event100,100)
C
      searchMoonIllum=searchTarget(Target,
     &     TargetCycle,getMoonIllum,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMoonIllum

      real function searchMoonPhase(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C     
      character*16 myname
      data myname /'searchMoonPhase'/
      external getMoonPhase
      real targetCycle,getMoonPhase
      parameter (targetCycle=100.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=3.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 30.0D0
      else
         tendJD = tstartJD - dtin - 30.0D0
      end if
C
      call chop0(event100,100)
C
      searchMoonPhase=searchTarget(Target,
     &     TargetCycle,getMoonPhase,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMoonPhase

      SUBROUTINE searchMoonPhaseHardy(f_searchMoonPhase,
     & target,
     & tstartJD,
     & secdec,
     & direction,
     & irc)
      implicit none

      REAL, INTENT(OUT) :: f_searchMoonPhase
      REAL, INTENT(IN) :: target
      REAL, INTENT(IN) :: tstartJD
      INTEGER, INTENT(IN) :: secdec
      INTEGER, INTENT(IN) :: direction
      INTEGER, INTENT(INOUT) :: irc


      INTEGER  :: maxrep
      data maxrep /1/
      INTEGER  :: nrep
      data nrep /0/
      REAL     :: repJD(1)
      CHARACTER*250 :: CRC250
      CHARACTER*250 :: rep250(1)
      CHARACTER*100 :: event100
      INTEGER :: I
      REAL    :: searchMoonPhase
      EXTERNAL searchMoonPhase

      f_searchMoonPhase =  searchMoonPhase(target,
     &                       tstartJD,
     &                       secdec,
     &                       maxrep,
     &                       nrep,
     &                       repJD,
     &                       rep250,
     &                       event100,
     &                       direction,
     &                       crc250,irc)

      END SUBROUTINE searchMoonPhaseHardy
      real function searchMoonRise(lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dt                   ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchMoonRise'/
      external getMoonElevation
      real getMoonElevation
C
      real target               ! target value of Moon celestial longtitude
      integer jj,inrep
      real targetCycle
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
C     
      call chop0(event100,100)
C
      Target= -refraction       ! target elevation
      dt=0.1D0                  ! initial time increment (days)
      inrep=nrep+1
      if(direction.eq.1) then
         tendJD=tstartJD+1.5D0
      else 
         tendJD=tstartJD-1.5D0
      end if
C
      searchMoonRise=searchLLTarget(Target,
     &     TargetCycle,getMoonElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dt,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,           ! elevation rises (decreases if you go backwards)
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C     
      do jj=inrep,nrep
         repJD(jj)=repJD(jj)-1.0D-10
      end do
      
      return
      end function searchMoonRise
      real function searchMoonSet(lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dt                   ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchMoonSet'/
      external getMoonElevation
      real getMoonElevation
C
      real target               ! target value of Moon celestial longtitude
      integer jj,inrep
      real targetCycle
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
C     
      call chop0(event100,100)
C
      Target= -refraction       ! target elevation
      dt=0.1D0                  ! initial time increment (days)
      inrep=nrep+1
      if(direction.eq.1) then
         tendJD=tstartJD+1.5D0
      else 
         tendJD=tstartJD-1.5D0
      end if
C
      searchMoonSet=searchLLTarget(Target,
     &     TargetCycle,getMoonElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dt,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     -direction,          ! elevation sets (just think about it...)
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C     
      do jj=inrep,nrep
         repJD(jj)=repJD(jj)-1.0D-10
      end do
      
      return
      end function searchMoonSet
      real function searchMoonUpperElevation(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer pitch             ! 1: rising, -1: falling, 0:any
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*26 myname
      data myname /'searchMoonUpperElevation'/
      external getMoonUpperElevation
      real targetCycle,getMoonUpperElevation
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      integer ipitch
C
      call chop0(event100,100)
C
      dtin=0.02D0
      if(direction.eq.1) then   ! forewards
         ipitch=pitch
         tendJD=tstartJD+1.5D0
      else                      ! backwards
         ipitch=-pitch
         tendJD=tstartJD-1.5D0
      end if
CC
      searchMoonUpperElevation=searchLLTarget(Target,
     &     TargetCycle,getMoonUpperElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     ipitch,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMoonUpperElevation

      SUBROUTINE searchMoonUpperElevationHardy(f_elevation,
     & target,
     & lat, lon, height,
     & tstartJD,
     & secdec,
     & pitch,
     & direction,
     & irc)
      implicit none

      REAL, INTENT(OUT) :: f_elevation
      REAL, INTENT(IN) :: target, lat, lon, height
      REAL, INTENT(IN) :: tstartJD
      INTEGER, INTENT(IN) :: secdec
      INTEGER, INTENT(IN) :: pitch, direction
      INTEGER, INTENT(INOUT) :: irc


      INTEGER  :: maxrep
      data maxrep /1/
      INTEGER  :: nrep
      REAL     :: repJD(1)
      CHARACTER*250 :: CRC250
      CHARACTER*250 :: rep250(1)
      CHARACTER*100 :: event100
      INTEGER :: I
      REAL    :: searchMoonUpperElevation
      EXTERNAL searchMoonUpperElevation

      nrep = 0

      f_elevation =  searchMoonUpperElevation(target,
     &                       lat, lon, hgt, tstartJD,
     &                       secdec,
     &                       maxrep,
     &                       nrep,
     &                       repJD,
     &                       rep250,
     &                       event100,
     &                       pitch, direction,
     &                       crc250,irc)

      END SUBROUTINE searchMoonUpperElevationHardy
      real function searchPolarSunAngle(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchPolarSunAngle'/
      external getPolarSunAngleX
      real targetCycle,getPolarSunAngleX
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=30.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 369.0D0
      else
         tendJD = tstartJD - dtin - 369.0D0
      end if
C
      call chop0(event100,100)
C
      searchPolarSunAngle=searchLLTarget(Target,
     &     TargetCycle,getPolarSunAngleX,
     &     lat, lon, hgt, 
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,
     &     repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
      return
      end function searchPolarSunAngle

      real function searchPolarMoonAngle(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchPolarMoonAngle'/
      external getPolarMoonAngleX
      real targetCycle,getPolarMoonAngleX
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=3.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 365.0D0
      else
         tendJD = tstartJD - dtin - 365.0D0
      end if
C
      call chop0(event100,100)
C
      searchPolarMoonAngle=searchLLTarget(Target,
     &     TargetCycle,getPolarMoonAngleX,
     &     lat, lon, hgt, 
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchPolarMoonAngle

      real function searchSaturnLon(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'searchSaturnLon'/
      external getSaturnLon
      real targetCycle,getSaturnLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
C
      dtin=40.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 450.0D0
      else
         tendJD = tstartJD - dtin - 450.0D0
      end if
C
      call chop0(event100,100)
C
      searchSaturnLon=searchTarget(Target,
     &     TargetCycle,getSaturnLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchSaturnLon

      real function searchSolarTime(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchSolarTime'/
      external getSolarTime
      real targetCycle,getSolarTime
      parameter (targetCycle=24.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=0.1D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 1.5D0
      else
         tendJD = tstartJD - dtin - 1.5D0
      end if
C
      call chop0(event100,100)
C
      searchSolarTime=searchLLTarget(Target,
     &     TargetCycle,getSolarTime,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchSolarTime

      SUBROUTINE searchSolarTimeHardy(f_time,
     & target,
     & lat, lon, height,
     & tstartJD,
     & secdec,
     & direction,
     & irc)
      implicit none

      REAL, INTENT(OUT) :: f_time
      REAL, INTENT(IN) :: target, lat, lon, height
      REAL, INTENT(IN) :: tstartJD
      INTEGER, INTENT(IN) :: secdec
      INTEGER, INTENT(IN) :: direction
      INTEGER, INTENT(INOUT) :: irc


      INTEGER  :: maxrep
      data maxrep /1/
      INTEGER  :: nrep
      REAL     :: repJD(1)
      CHARACTER*250 :: CRC250
      CHARACTER*250 :: rep250(1)
      CHARACTER*100 :: event100
      INTEGER :: I
      REAL    :: searchSolarTime
      EXTERNAL searchSolarTime
      
      nrep = 0

      f_time =  searchSolarTime(target,
     &                       lat, lon, height, tstartJD,
     &                       secdec,
     &                       maxrep,
     &                       nrep,
     &                       repJD,
     &                       rep250,
     &                       event100,
     &                       direction,
     &                       crc250,irc)


      END SUBROUTINE searchSolarTimeHardy
      real function searchSunAzimuth(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchSunAzimuth'/
      external getSunAzimuth
      real targetCycle,getSunAzimuth
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=0.1D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 1.5D0
      else
         tendJD = tstartJD - dtin - 1.5D0
      end if
C
      call chop0(event100,100)
C
      searchSunAzimuth=searchLLTarget(Target,
     &     TargetCycle,getSunAzimuth,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchSunAzimuth

      real function searchSunEclipse(
     &     lat,lon,hgt,
     &     tstartJDin,secdec,
     &     loccMax,occMax,loccMin,occMin,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real lat,lon,hgt
      real tstartJDin         ! start time
      integer secdec            ! number of second-decimals
      logical loccMax
      real occMax
      logical loccMin
      real occMin
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repId(max(1,maxrep)) ! output report identification (SEE TABLE ABOVE)
      real repVal(max(1,maxrep)) ! output report value (SEE TABLE ABOVE)
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! search direction
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C     
      character*18 myname
      data myname /'searchSunEclipse'/
      external getMoonPhase,getSunEclipse,getSunEclipseOcc
      external getSunOcc
      real getMoonPhase,targetCycle,getSunEclipse,getSunEclipseOcc
      real getSunOcc
      parameter (targetCycle=100.0D0) ! target value which correponds to 0.0D0
      real dtin                 ! initial time increment
      real Rm,Rp,Ru,xCycle,xJD,pJD,dt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer xrep,maxxrep,yy1,mm1,dd1,hh1,mi1
      real sec1
      real target               ! target value of Sun celestial longtitude
      character*100 xevent100
      integer xeventId
      integer length,lend,leno,lent
      external length
      character*16 t16
      character*30 t30
      character*100 fmt100
      logical bdone
      real occ,fact
      real tendJDin           ! end time
      logical proceed
      integer cnt,cntmax
      parameter (cntmax=10000)
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
      external getSunElevation
      real getSunElevation
C     
      searchSunEclipse=0.0D0
      call chop0(event100,100)
      lend=length(event100,100,10)
C
      if (direction.eq.1) then
         tendJDin = tstartJDin + 15.0D0*365.0D0
      else
         tendJDin = tstartJDin - 15.0D0*365.0D0
      end if
C
      Rm =  1738.0D0            ! moon radius
      bdone=.false.
      Target=0.0D0              ! SunEclipse entered at new moon
      tstartJD=tstartJDin
      dtin=sign(min(33.0D0,abs(tendJDin-tstartJDin)),
     &     tendJDin-tstartJDin)
      tendJD=tstartJDin + dtin
      cnt=0
      do while (cnt.lt.cntmax.and..not.bdone)
         cnt=cnt+1
         dt=dtin/10.0D0
         tendJD=tstartJD + dtin
         maxxrep=0
         xrep=0
         xJD=searchTarget(Target, ! only get time of 
     &        TargetCycle,getMoonPhase,
     &        tstartJD,tendJD,dt,secdec,
     &        eventId,event100,maxxrep,xrep,repJD,repid,repval,rep250,
     &        0,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from searchTarget.'
            call chop0(crc250,250)
            return
         end if
C         write(*,*)myname,'Phase:',getMoonPhase(tstartJD,crc250,irc),
C     &           tstartJD,xrep.gt.0
         if (xrep.gt.0) then    ! found a match, search backwards for sunEclipse entry
            tstartJD=xJD
            maxxrep=0
            xrep=0
            tstartJD=searchLLMax( ! only get time of 
     &           getSunOcc,
     &           lat,lon,hgt,
     &           tstartJD-0.5D0,tstartJD+0.5D0,0.01D0,secdec,
     &           eventId,event100,
     &           maxxrep,xrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from searchLLMax.'
               call chop0(crc250,250)
               return
            end if
            Rp=getSunEclipse(tstartJD,lat,lon,hgt,crc250,irc) ! distance between Moon center and earth-pensunEclipse
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from GetSunEclipse.'
               call chop0(crc250,250)
               return
            end if
            proceed=(Rp.lt.Rm)
            occ=getSunEclipseOcc()
            CALL JD2DATE(tstartJD,
     &           YY1,
     &           MM1,
     &           DD1,
     &           HH1,
     &           MI1,
     &           SEC1) 
            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888        FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
            if (proceed) then
               proceed=(proceed.and.(.not.loccMin.or.occ.ge.occMin))
               proceed=(proceed.and.(.not.loccMax.or.occ.le.occMax))
            end if
C     find out if sun is visible at maximum occultation...
            if (proceed) then
               proceed=(getSunElevation(tstartJD,lat,lon,hgt,
     &              crc250,irc).gt.-refraction) ! uppder disk limb above horison
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from getSunElevation.'
                  call chop0(crc250,250)
                  return
               end if
            end if
            if (proceed) then   ! we have a lunar eclipse
C     write(*,*)myname,'Match.',t16,tstartJD
               searchSunEclipse=tstartJD ! returns center-time of eclipse
               bdone=.true.     ! we found a match and we are done
C     search backwards for Rp .eq. Rm (PENSUNECLIPSE PARTIAL START)
               dt=-0.2D0        ! eclipse starts not more than 1 day earlier
               xeventId=eventId + 0
               xevent100=event100(1:lend)//' PARTIAL STARTS'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=max(min(tstartJDin,tendJDin),
     &              tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchLLTarget(Rm, ! find start
     &              xCycle,getSunEclipse,
     &              lat,lon,hgt,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchLLTarget.'
                  call chop0(crc250,250)
                  return
               end if               
C     write(*,*) myname,'Ru:',rp,rm,rp.le.rm
               if (Rp.le.-Rm) then               
C     search backwards for Ru .eq. Rm (TOTAL ECLIPSE START)
                  dt=-0.2D0     ! eclipse starts not more than 1 day earlier
                  xeventId=eventId + 1
                  xevent100=event100(1:lend)//' TOTAL STARTS'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=max(min(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchLLTarget(-Rm, ! find start
     &                 xCycle,getSunEclipse,
     &                 lat,lon,hgt,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchLLTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
C     report max occulation
               nrep=nrep+1
               if (nrep.le.maxrep) then
                  if (secdec.gt.0) then
                     write(fmt100,'(A,I2.2,A,I1,A)')
     &                    "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                    "I2.2,':',I2.2,F",
     &                    secdec+1,".",secdec,")"
                     fact=10.0D0**(real(secdec))
                     write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                    int(sec1),real(int((sec1-int(sec1))*
     &                    fact))/fact
                  else
                     write(fmt100,'(A,I2.2,A)')
     &                    "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                    "I2.2,':',I2.2)"
                     write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                    int(sec1)
                  end if
                  call chop0(T30,30)
                  lent=length(T30,30,30)
                  lend=length(event100,100,30)
                  write(rep250(nrep),
     &                 '(A,X,A," MAX OCCULTATION=",F8.1,"%")')
     &                 T30(1:lent),event100(1:lend),
     &                 occ
                  repJD(nrep)=tstartJD
                  repid(nrep)=eventId + 2
                  repval(nrep)=occ
               end if
               if (Rp.le.-Rm) then ! Moon totally within Sun cone
C     search forwards for Ru .eq. -Rm (TOTAL ECLIPSE STOP)
                  dt=0.2D0      ! eclipse stops not more than 1 day later
                  xeventId=eventId + 3
                  xevent100=event100(1:lend)//
     &                 ' TOTAL STOPS'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=min(max(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchLLTarget(-Rm, ! find stop
     &                 xCycle,getSunEclipse,
     &                 lat,lon,hgt,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchLLTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
C     search forwards for Rp .eq. -Rm (PARTIAL ECLIPSE STOP)
               dt=0.2D0         ! eclipse stops not more than 1 day later
               xeventId=eventId + 4
               xevent100=event100(1:lend)//' PARTIAL STOPS'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=min(max(tstartJDin,tendJDin),
     &              tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchLLTarget(Rm, ! find stop
     &              xCycle,getSunEclipse,
     &              lat,lon,hgt,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchLLTarget.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         if ((dtin.gt.0.0D0.and.tstartJD+0.5*dtin
     &        .gt.tendJDin).or.
     &        (dtin.lt.0.0D0.and.tstartJD+0.5*dtin
     &        .lt.tendJDin)) then
            bdone=.true.
         else
            tstartJD=tstartJD+dtin*0.5D0 ! skip fast forward
         end if
      end do
C     
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
C     
      return
      end function searchSunEclipse
      real function searchSunElevation(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer pitch             ! 1: rising, -1: falling, 0:any
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchSunElevation'/
      external getSunUpperElevation
      real targetCycle,getSunUpperElevation
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      integer ipitch
C
      call chop0(event100,100)
C
      dtin=0.02D0
      if(direction.eq.1) then
         ipitch=pitch
         tendJD=tstartJD+1.5D0
      else 
         ipitch=-pitch
         tendJD=tstartJD-1.5D0
      end if
C
      searchSunElevation=searchLLTarget(Target,
     &     TargetCycle,getSunUpperElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     ipitch,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchSunElevation

      real function searchSunRA(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*16 myname
      data myname /'searchSunRA'/
      external getSunRA
      real targetCycle,getSunRA
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                 ! initial step length
      real tendJD             ! end time in JD
C
C
      dtin=30.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 369.0D0
      else
         tendJD = tstartJD - dtin - 369.0D0
      end if
      call chop0(event100,100)
C
      searchSunRA=searchTarget(Target,
     &     TargetCycle,getSunRA,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchSunRA


      real function searchMoonRA(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*16 myname
      data myname /'searchMoonRA'/
      external getMoonRA
      real targetCycle,getMoonRA
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                 ! initial step length
      real tendJD             ! end time in JD
C
      dtin=3.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 369.0D0
      else
         tendJD = tstartJD - dtin - 369.0D0
      end if
      call chop0(event100,100)
C
      searchMoonRA=searchTarget(Target,
     &     TargetCycle,getMoonRA,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchMoonRA

      real function searchSunRise(lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dt                   ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchSunRise'/
      external getSunElevation
      real getSunElevation
C
      real target               ! target value of Sun celestial longtitude
      integer jj,inrep
      real targetCycle
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
C     
      call chop0(event100,100)
C
      Target= -refraction       ! target elevation
      dt=0.1D0                  ! initial time increment (days)
      inrep=nrep+1
      if(direction.eq.1) then
         tendJD=tstartJD+1.5D0
      else 
         tendJD=tstartJD-1.5D0
      end if
C
      searchSunRise=searchLLTarget(Target,
     &     TargetCycle,getSunElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dt,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,           ! elevation rises
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C     
      do jj=inrep,nrep
         repJD(jj)=repJD(jj)-1.0D-10
      end do
      
      return
      end function searchSunRise
      real function searchSunSet(lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real lat
      real lon
      real hgt
      real tstartJD           ! start time
      real tendJD             ! end time
      real dt                   ! initial time increment
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep             ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchSunSet'/
      external getSunElevation
      real getSunElevation
C
      real target               ! target value of Sun celestial longtitude
      integer jj,inrep
      real targetCycle
      parameter (targetCycle=720.0D0) ! target value which correponds to 0.0D0
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
C     
      call chop0(event100,100)
C
      Target= -refraction       ! target elevation
      dt=0.1D0                  ! initial time increment (days)
      inrep=nrep+1
      if(direction.eq.1) then
         tendJD=tstartJD+1.5D0
      else 
         tendJD=tstartJD-1.5D0
      end if
C
      searchSunSet=searchLLTarget(Target,
     &     TargetCycle,getSunElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dt,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     -direction,          ! elevation sets
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C     
      do jj=inrep,nrep
         repJD(jj)=repJD(jj)-1.0D-10
      end do
      
      return
      end function searchSunSet
      real function searchSunUpperElevation(Target,
     &     lat,lon,hgt,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real lat
      real lon
      real hgt
      real tstartJD           ! start time in JD
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer pitch             ! 1: rising, -1: falling, 0:any
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*26 myname
      data myname /'searchSunUpperElevation'/
      external getSunUpperElevation
      real targetCycle,getSunUpperElevation
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      integer ipitch
C
      call chop0(event100,100)
C
      dtin=0.02D0
      if(direction.eq.1) then   ! forewards
         ipitch=pitch
         tendJD=tstartJD+1.5D0
      else                      ! backwards
         ipitch=-pitch
         tendJD=tstartJD-1.5D0
      end if
CC
      searchSunUpperElevation=searchLLTarget(Target,
     &     TargetCycle,getSunUpperElevation,
     &     lat,lon,hgt,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     ipitch,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchLLTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchSunUpperElevation

      SUBROUTINE searchSunUpperElevationHardy(f_elevation,
     & target,
     & lat, lon, height,
     & tstartJD,
     & secdec,
     & pitch,
     & direction,
     & irc)
      implicit none

      REAL, INTENT(OUT) :: f_elevation
      REAL, INTENT(IN) :: target, lat, lon, height
      REAL, INTENT(IN) :: tstartJD
      INTEGER, INTENT(IN) :: secdec
      INTEGER, INTENT(IN) :: pitch, direction
      INTEGER, INTENT(INOUT) :: irc


      INTEGER  :: maxrep
      data maxrep /1/
      INTEGER  :: nrep
      REAL     :: repJD(1)
      CHARACTER*250 :: CRC250
      CHARACTER*250 :: rep250(1)
      CHARACTER*100 :: event100
      INTEGER :: I
      REAL    :: searchSunUpperElevation
      EXTERNAL searchSunUpperElevation

      nrep = 0

      f_elevation =  searchSunUpperElevation(target,
     &                       lat, lon, height, tstartJD,
     &                       secdec,
     &                       maxrep,
     &                       nrep,
     &                       repJD,
     &                       rep250,
     &                       event100,
     &                       pitch, direction,
     &                       crc250,irc)

      END SUBROUTINE searchSunUpperElevationHardy
      real function searchTarget(tval,
     &     tValCycle,funk,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     pitch,
     &     crc250,irc)
      implicit none
      real tVal                 ! target value 
      real tValCycle            ! target value cycle (360.0D0 for degrees)
      real funk                 ! function giving the value
      external funk
      real tstartJD           ! start time in Julian day JD
      real tendJD             ! end time in Julian day JD
      real dtin                 ! time interval (absolute value)
      integer secdec            ! number of decimals in seconds output
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! maximum number of reports
      integer nrep              ! number of reports so far
      real repJD(max(1,maxrep)) ! report time in Julian day JD
      integer repid(max(1,maxrep)) ! report time in Julian day JD
      real repval(max(1,maxrep)) ! report time in Julian day JD
      character*250 rep250(max(1,maxrep)) ! report strings
      integer pitch             ! 1: rising, -1: falling, 0:any
      character*250 crc250      ! error description
      integer irc               ! error return code (0: ok)
      integer lencrc
C
      character*14 myname
      data myname/'searchTarget'/
C
      character*250 buff250
      integer YY1,MM1,DD1,HH1,MI1
      real sec1,idt,mindt,fact,dt
      real JD,val,val1,jstop
      real xJD(3),xval(3),dval(3)
      logical major,first
      character*100 fmt100
      character*30 T30
      character*16 t16
      real ndt
      integer length,lend,lent
      externale length
      real diffcycle
      external diffcycle
      logical bdeb,bdone
      data bdeb/.false./
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      if (bdeb)write(*,*)myname,'Entering.',irc
      lend=length(event100,100,20)
      JD=tstartJD
      searchTarget=tstartJD
      mindt=(10.0D0**(-real(secdec)))/(3.33D0*86400D0)
      ndt=nint(0.50001D0+abs(tendJD-tstartJD)/max(1.0D-5,abs(dtin)))
      dt=abs(tendJD-tstartJD)/ndt
      dt=sign(max(6.9D-3,abs(dt)),tendJD-tstartJD)
      if (bdeb)write(*,*)myname,'Interval:',tstartJD,tendJD,dt
      if (bdeb)write(*,*)myname,'Range:',JD,tendJD,mindt,dtin
      searchTarget=tendJD
      first=.true.
      bdone=.false.
      cnt=0
      do while (cnt.lt.cntmax.and.(.not.bdone.and.(
     &     (dt.gt.0.0D0.and.JD.le.tendJD).or.
     &     (dt.lt.0.0D0.and.JD.ge.tendJD))))
         cnt=cnt+1
         CALL JD2DATE(JD,
     &        YY1,
     &        MM1,
     &        DD1,
     &        HH1,
     &        MI1,
     &        SEC1) 
         WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888     FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16
C
C     get new value
C
         val=funk(JD,crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from FUNK.'
            call chop0(crc250,250)
            return
         end if
C
         if (bdeb)write(*,'(4(X,A),2(X,F20.8))')
     &        myname,'Step:',T16,event100(1:lend),val,tval
C         write(*,*)myname,'JNK:',xval(1),xval(2),val,val1
C
C     set boundaries (max/min)
C
         if (first.or.major) then
            xJD(1)=xJD(2)
            xJD(2)=JD
            xval(1)=xval(2)
            xval(2)=val
         end if
C
         dval(1)=diffCycle(xval(1),tval,tValCycle)
         dval(2)=diffCycle(xval(2),tval,tValCycle)
         dval(3)=diffCycle(val,tval,tValCycle)
C     
C     determine if we are in major/minor loop, and increment time
C
         if (first) then
            first=.false.
            idt=dt
            major=.true.        ! we always start in major loop
         else if (major) then   ! check if we must switch to minor loop
            major= (pitch.eq.-1.and.dval(2).gt.0.0D0).or.
     &           (pitch.eq.1.and.dval(1).gt.0.0D0) .or.
     &           ((dval(1).gt.0.0D0 .and. dval(2).gt.0.0D0).or.
     &           (dval(1).lt.0.0D0 .and. dval(2).lt.0.0D0)).or.
     &           (tvalcycle.gt.0.0D0.and.
     &           abs(dval(1)-dval(2)).gt.tValCycle*0.75D0)

            if (.not.major) then ! we have just switched to minor loop
               if (bdeb)write(*,*)myname,'Entered minor loop.',dval(1),
     &              dval(2),dval(3),idt,xval(1),xval(2),val,tval
               xJD(3)=xJD(2)
               xval(3)=xval(2)
               idt= -0.5D0 * (xJD(2)-xJD(1))


            end if
         else if (abs(idt).lt.mindt) then ! we have completed the minor loop
            if (abs(dval(1)-dval(2)).lt.tValCycle*0.25D0 ! check if discontinuity-hit
     &           .or.tValCycle.lt.0.0D0) then
               nrep=nrep+1
               if (maxrep.ne.0) then ! make report
                  if (nrep.le.maxrep) then
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))
     &                       *fact))/fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     write(rep250(nrep),'(A,X,A)')T30(1:lent),
     &                    event100(1:lend)
                     repJD(nrep)=JD
                     repid(nrep)=eventId
                     repval(nrep)=-99999.0D0
                     if (bdeb)write(*,*)myname,'Ended minor loop.',idt
                  end if
               end if
               searchTarget=JD
               bdone=.true.
            end if
C     reset variables
            major=.true.
            xJD(2)=xJD(3)
            xval(2)=xval(3)
            JD=xJD(3)
            idt=dt
         else                   ! we are in minor loop
C            write(*,*)myname,'Minor before:',dval(1),dval(2),dval(3),idt
            if ((dval(3).gt.0.0D0.and.dval(1).gt.0.0D0).or.
     &           (dval(3).lt.0.0D0.and.dval(1).lt.0.0D0)) then
               xJD(1)=xJD(2) ! swap
               xval(1)=xval(2)
               dval(1)=dval(2)
            end if
            xJD(2)=JD
            xval(2)=val
            dval(2)=dval(3)
            idt= -0.5D0 * (xJD(2)-xJD(1)) ! go back half-a-step
C            write(*,*)myname,'Minor  after:',
C     &         dval(1),dval(2),dval(3),idt
         end if
C     increment time
         JD=JD+idt
      end do
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
      if (nrep.gt.maxrep.AND.MAXREP.NE.0) then
         write(buff250,*)
     &        ' < '//myname,' > Too many reports.',nrep,maxrep
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=471
         return
      end if
C
      if (bdeb)write(*,*)myname,'Done.',irc
      return
      end function searchTarget
C
      real function searchUmbra(
     &     tstartJDin,secdec,
     &     loccMax,occMax,loccMin,occMin,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJDin         ! start time
      integer secdec            ! number of second-decimals
      logical loccMax
      real occMax
      logical loccMin
      real occMin
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repId(max(1,maxrep)) ! output report identification (SEE TABLE ABOVE)
      real repVal(max(1,maxrep)) ! output report value (SEE TABLE ABOVE)
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! search direction (+1: next, -1: previous)
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C     
      character*18 myname
      data myname /'searchUmbra'/
      logical bdeb
      data bdeb/.true./
      external getMoonPhase,getUmbra,getPenUmbra,getMoonOcc,getUmbraOcc
      real getMoonPhase,getUmbra,getPenUmbra,getMoonOcc,getUmbraOcc
      real targetCycle
      parameter (targetCycle=100.0D0) ! target value which correponds to 0.0D0
      real dtin                 ! initial time increment
      real Rm,Rp,Ru,xCycle,xJD,pJD,dt
      real tstartJD           ! start time
      real tendJD             ! end time
      integer xrep,maxxrep,yy1,mm1,dd1,hh1,mi1
      real sec1
      character*100 xevent100
      integer xeventId
      integer length,lend,leno,lent
      external length
      character*16 t16
      character*30 t30
      character*100 fmt100
      logical bdone
      real occ,fact
      real tendJDin           ! end time
      logical proceed
      integer cnt,cntmax
      parameter (cntmax=10000)
C     
      searchUmbra=0.0D0
      call chop0(event100,100)
      lend=length(event100,100,10)
C
      if (direction.eq.1) then
         tendJDin = tstartJDin + 15.0D0*365.0D0
      else
         tendJDin = tstartJDin - 15.0D0*365.0D0
      end if
C
      Rm =  1738.0D0            ! moon radius
      bdone=.false.
      Target=50.0D0             ! Umbra entered at full moon
      tstartJD=tstartJDin
      dtin=sign(min(33.0D0,abs(tendJDin-tstartJDin)),
     &     tendJDin-tstartJDin)
      tendJD=tstartJDin + dtin
      cnt=0
      do while (cnt.lt.cntmax.and..not.bdone)
         cnt=cnt+1
         dt=dtin/10.0D0
         tendJD=tstartJD + dtin
         maxxrep=0
         xrep=0
         tstartJD=searchTarget(Target, ! only get time of 
     &        TargetCycle,getMoonPhase,
     &        tstartJD,tendJD,dt,secdec,
     &        eventId,event100,maxxrep,xrep,repJD,repid,repval,rep250,
     &        0,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from searchTarget.'
            call chop0(crc250,250)
            return
         end if
         if (xrep.gt.0) then    ! found a match, search backwards for umbra entry
C            write(*,*)myname,' Time A:', tstartJD
            maxxrep=0
            xrep=0
            tstartJD=searchMin( ! only get time of 
     &           getPenumbra,
     &           tstartJD-0.5D0,tstartJD+0.5D0,0.01D0,secdec,
     &           eventId,event100,
     &           maxxrep,xrep,repJD,repid,repval,rep250,
     &           crc250,irc)
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from searchMax.'
               call chop0(crc250,250)
               return
            end if

C            write(*,*)myname,' Time B:', tstartJD
            Rp=getPenumbra(tstartJD,crc250,irc) ! distance between Moon center and earth-penumbra
            if (irc.ne.0) then
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//' < '//myname//
     &              ' > Error return from GetPenumbra.'
               call chop0(crc250,250)
               return
            end if
            proceed=(Rp.lt.Rm)
            CALL JD2DATE(tstartJD,
     &           YY1,
     &           MM1,
     &           DD1,
     &           HH1,
     &           MI1,
     &           SEC1) 
            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888        FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16

C            write(*,*)myname,'REP:  ',t16,xrep,proceed

            if (proceed) then
               Ru=getUmbra(tstartJD,crc250,irc) ! distance between Moon center and earth-penumbra
               occ=getUmbraOcc()
               proceed=(proceed.and.(.not.loccMin.or.occ.ge.occMin))
               proceed=(proceed.and.(.not.loccMax.or.occ.le.occMax))
            end if
            if (proceed) then   ! we have a lunar eclipse
               searchUmbra=tstartJD ! returns center-time of eclipse
               bdone=.true.     ! we found a match and we are done
C     search backwards for Rp .eq. Rm (PENUMBRA PARTIAL START)
               dt=-0.2D0        ! eclipse starts not more than 1 day earlier
               xeventId=eventId + 0
               xevent100=event100(1:lend)//
     &              ' PENUMBRA CONTACT STARTS (P1)'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=max(min(tstartJDin,tendJDin),
     &              tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getPenumbra,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
               
C     write(*,*) myname,'Ru:',rp,rm,rp.le.rm
               if (Ru.le.Rm) then               
C     search backwards for Ru .eq. Rm (UMBRA PARTIAL START)
                  dt=-0.2D0     ! eclipse starts not more than 1 day earlier
                  xeventId=eventId + 1
                  xevent100=event100(1:lend)//
     &                 ' UMBRA CONTACT STARTS (U1)'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=max(min(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchTarget(Rm, ! find start
     &                 xCycle,getUmbra,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
               if (Ru.lt.-Rm) then ! Moon totally within Earth umbra
C     search backwards for Ru .eq. -Rm (UMBRA TOTAL START)
                  dt=-1.0D0     ! eclipse starts not more than 1 day earlier
                  xeventId=eventid + 2
                  xevent100=event100(1:lend)//
     &                 ' TOTAL ECLIPSE STARTS (U2)'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=max(min(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchTarget(-Rm, ! find start
     &                 xCycle,getUmbra,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
               if (Ru.le.Rm) then !moon in earth umbra
C     report max occulation
                  nrep=nrep+1
                  if (nrep.le.maxrep) then
                     if (secdec.gt.0) then
                        write(fmt100,'(A,I2.2,A,I1,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2,F",
     &                       secdec+1,".",secdec,")"
                        fact=10.0D0**(real(secdec))
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1),real(int((sec1-int(sec1))*
     &                       fact))/fact
                     else
                        write(fmt100,'(A,I2.2,A)')
     &                       "(I4,'/',I2.2,'/',I2.2,X,I2.2,':',"//
     &                       "I2.2,':',I2.2)"
                        write(T30,FMT=FMT100) YY1,MM1,DD1,HH1,MI1,
     &                       int(sec1)
                     end if
                     call chop0(T30,30)
                     lent=length(T30,30,30)
                     lend=length(event100,100,30)
                     write(rep250(nrep),
     &                    '(A,X,A," MAX OCCULTATION=",F8.1,"%")')
     &                    T30(1:lent),event100(1:lend),
     &                    occ
                     repJD(nrep)=tstartJD
                     repid(nrep)=eventId + 3
                     repval(nrep)=occ
                  end if
               end if
               if (Ru.lt.-Rm) then ! Moon totally within Earth umbra
C     search forwards for Ru .eq. -Rm (UMBRA TOTAL STOP)
                  dt=0.2D0      ! eclipse stops not more than 1 day later
                  xeventId=eventId + 4
                  xevent100=event100(1:lend)//
     &                 ' TOTAL ECLIPSE STOPS (U3)'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=min(max(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchTarget(-Rm, ! find stop
     &                 xCycle,getUmbra,
     &                 tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchTarget.'
                     call chop0(crc250,250)
                     return
                  end if
               end if
C     search forwards for Ru .eq. Rm (UMBRA PARTIAL STOP)
               if (Ru.lt.Rm) then
                  dt=1.0D0      ! eclipse stops not more than 1 day later
                  xeventId=eventId + 5
                  xevent100=event100(1:lend)//
     &                 ' UMBRA CONTACT STOPS (U4)'
                  call chop0(xevent100,100)
                  xCycle=-1.0D0 ! disable variable cycling
                  tendJD=min(max(tstartJDin,tendJDin),
     &                 tstartJD+dt)
                  dt=dt*0.1D0
                  xJD=searchTarget(Rm, ! find stop
     &                 xCycle,getUmbra,
     &                 tstartJD,tendJD,dt,secdec,xeventid,xevent100,
     &                 maxrep,nrep,repJD,repid,repval,rep250,
     &                 0,
     &                 crc250,irc)
                  if (irc.ne.0) then
                     lencrc=length(crc250,250,10)
                     crc250=crc250(1:lencrc)//' < '//myname//
     &                    ' > Error return from searchTarget.'
                     call chop0(crc250,250)
                     return
                  end if
C     write(*,*) 'REPORTMP:',t16,xJD,nrep,rm
               end if
C     search forwards for Rp .eq. -Rm (PENUMBRA TOTAL STOP)
               dt=0.2D0         ! eclipse stops not more than 1 day later
               xeventId=eventId + 6
               xevent100=event100(1:lend)//
     &              ' PENUMBRA CONTACT STOPS (P2)'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=min(max(tstartJDin,tendJDin),
     &              tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find stop
     &              xCycle,getPenumbra,
     &              tstartJD,tendJD,dt,secdec,xeventid,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
            end if
         end if
         if ((dtin.gt.0.0D0.and.tstartJD+0.5*dtin
     &        .gt.tendJDin).or.
     &        (dtin.lt.0.0D0.and.tstartJD+0.5*dtin
     &        .lt.tendJDin)) then
            bdone=.true.
         else
            tstartJD=tstartJD+dtin*0.5D0 ! skip fast forward
         end if
      end do
C     
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
C     
      return
      end function searchUmbra
      real function searchVenusRelLon(Target,
     &     tstartJD,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
C
      implicit none
      real target               ! target value
      real tstartJD           ! start time in JD
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep))      ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! +1: next, -1: previous
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*18 myname
      data myname /'searchVenusRelLon'/
      external getVenusRelLon
      real targetCycle,getVenusRelLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real tendJD             ! end time in JD
      real dtin                 ! initial step length
C
      dtin=60.0D0
      if (direction.eq.1) then
         tendJD = tstartJD + dtin + 600.0D0
      else
         tendJD = tstartJD - dtin - 600.0D0
      end if
C
      call chop0(event100,100)
C
      searchVenusRelLon=searchTarget(Target,
     &     TargetCycle,getVenusRelLon,
     &     tstartJD,tendJD,dtin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     0,
     &     crc250,irc)
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from searchTarget.'
         call chop0(crc250,250)
         return
      end if
C
C      
      return
      end function searchVenusRelLon

      real function searchVenusTransit(
     &     tstartJDin,secdec,
     &     eventId,event100,maxrep,nrep,repJD,repid,repval,rep250,
     &     direction,
     &     crc250,irc)
      implicit none
      real target               ! target value of Sun celestial longtitude
      real tstartJDin         ! start time
      integer secdec            ! number of second-decimals
      integer eventId           ! event identification
      character*100 event100     ! search description
      integer maxrep            ! max number of reports
      integer nrep              ! current number of reports
      real repJD(max(1,maxrep)) ! event times in reports
      integer repid(max(1,maxrep)) ! report identification
      real repval(max(1,maxrep)) ! report value
      character*250 rep250(max(1,maxrep)) ! reports
      integer direction         ! direction of search
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      character*20 myname
      data myname /'searchVenusTransit'/
      external getVenusRelLon,getVenusTransit
      real targetCycle,getVenusTransit,getVenusRelLon
      parameter (targetCycle=360.0D0) ! target value which correponds to 0.0D0
      real dtin                   ! initial time increment
      real Rm,Rp,xCycle,xJD,pJD,dt
      real tstartJD         ! start time
      real tendJD           ! end time
      integer xrep,maxxrep,yy1,mm1,dd1,hh1,mi1
      real sec1
      character*100 xevent100
      integer xeventId
      integer length,lend
      external length
      character*16 t16
      real tendJDin           ! end time
      logical bdone
      integer cnt,cntmax
      parameter (cntmax=10000)
C
      call chop0(event100,100)
C
      if (direction.eq.1) then
         tendJDin = tstartJDin + 250.0D0*365.0D0
      else
         tendJDin = tstartJDin - 250.0D0*365.0D0
      end if
C
      searchVenusTransit=0.0D0
      Rm =   6051.0D0           ! venus radius
      bdone=.false.
      Target=0.0D0              ! Penumbra entered at venus inferior conjunction
      tstartJD=tstartJDin
      dtin=sign(min(600.0D0,abs(tendJDin-tstartJDin)),
     &     tendJDin-tstartJDin)
      tendJD=tstartJDin + dtin
      cnt=0
      do while (cnt.lt.cntmax.and..not.bdone)
         cnt=cnt+1
C     
         dt=dtin/10.0D0
         tendJD=tstartJD + dtin

         maxxrep=0
         xrep=0
         tstartJD=searchTarget(Target, ! only get time of 
     &        TargetCycle,getVenusRelLon,
     &        tstartJD,tendJD,dt,secdec,
     &        eventId,event100,maxxrep,xrep,repJD,repid,repval,rep250,
     &        0,
     &        crc250,irc)
         if (irc.ne.0) then
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//' < '//myname//
     &           ' > Error return from searchTarget.'
            call chop0(crc250,250)
            return
         end if

         if (xrep.gt.0) then    ! found a match, search backwards for penumbra entry

            CALL JD2DATE(tstartJD,
     &           YY1,
     &           MM1,
     &           DD1,
     &           HH1,
     &           MI1,
     &           SEC1) 
            WRITE(T16,FMT=888) YY1,MM1,DD1,HH1,MI1
 888        FORMAT(I4,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2) ! 16

            Rp=getVenusTransit(tstartJD,crc250,irc) ! distance between Venus and earth-sun cone

C           write(*,*) myname,'Rp:',T16,rp,rm,rp.le.rm
            if (Rp.le.Rm) then  ! Venus eclipses the Sun
               lend=length(event100,100,10)
               xrep=nrep
               dt=10.0D0       ! eclipse starts not more than 10 day earlier
               xeventId=eventId + 1
               xevent100=event100(1:lend)//' STOP'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=min(max(tstartJDin,tendJDin),tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find stop
     &              xCycle,getVenusTransit,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
               dt=-10.0D0       ! eclipse starts not more than 10 day earlier
               xeventId=eventId + 0
               xevent100=event100(1:lend)//' START'
               call chop0(xevent100,100)
               xCycle=-1.0D0    ! disable variable cycling
               tendJD=max(min(tstartJDin,tendJDin),tstartJD+dt)
               dt=dt*0.1D0
               xJD=searchTarget(Rm, ! find start
     &              xCycle,getVenusTransit,
     &              tstartJD,tendJD,dt,secdec,xeventId,xevent100,
     &              maxrep,nrep,repJD,repid,repval,rep250,
     &              0,
     &              crc250,irc)
               if (irc.ne.0) then
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//' < '//myname//
     &                 ' > Error return from searchTarget.'
                  call chop0(crc250,250)
                  return
               end if
               if (nrep.ne.xrep) then ! we are done...
                  searchVenusTransit=xJD
                  return
               end if
               return

C               write(*,*) 'REPORTMP:',t16,xJD,nrep,rm
            end if
         end if
         if ((dtin.gt.0.0D0.and.tstartJD+0.5*dtin
     &        .gt.tendJDin).or.
     &        (dtin.lt.0.0D0.and.tstartJD+0.5*dtin
     &        .lt.tendJDin)) then
            bdone=.true.
         else
            tstartJD=tstartJD+dtin*0.5D0 ! skip fast forward
         end if
      end do
C
      if (cnt.eq.cntmax)then
         write(buff250,*)
     &        ' < '//myname,' > Too many iterations.'
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         irc=457
         return
      end if
C
      return
      end function searchVenusTransit
      end subroutine ASTROEVENT

      real function getAbs(pos) ! vector absolute value
      real pos(3)
      getAbs=dsqrt(pos(1)*pos(1)+
     &     pos(2)*pos(2)+
     &     pos(3)*pos(3))
      return
      end function getAbs

      real function getAngle(pos1,pos2) ! angle between vectors
      real pos1(3),pos2(3),R1,R2,pos1N(3),pos2N(3),acosdeg
      external acosdeg
      R1=getAbs(pos1)
      R2=getAbs(pos2)
      pos1n(1)=pos1(1)/r1
      pos1n(2)=pos1(2)/r1
      pos1n(3)=pos1(3)/r1
      pos2n(1)=pos2(1)/r2
      pos2n(2)=pos2(2)/r2
      pos2n(3)=pos2(3)/r2
      getAngle=acosdeg(getDot(pos1n,pos2n))
      return
      end function getAngle

      real function getDot(pos1,pos2) ! dot product
      real pos1(3),pos2(3)
      getDot=pos1(1)*pos2(1)+pos1(2)*pos2(2)+pos1(3)*pos2(3)
      return
      end function getDot

      real function getJupiterLon(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real jupiterPos(6)
      real atan2deg,jupiterAng,sunAng,ang
      external atan2deg
      integer length
      external length
C
      character*16 myname
      data myname /'getJupiterLon'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephJupiterMJ(jupiterPos,crc250,irc) ! retrieve Jupiter geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephJupiterMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(jupiterPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,jupiterPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,jupiterPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate angle between sun and jupiter pos as seen from the earth
C
      jupiterAng=atan2deg(jupiterPos(2),jupiterPos(1))
      sunAng=atan2deg(sunPos(2),sunPos(1))
      ang=(jupiterAng-sunAng)
      ang=ang-real(nint((ang/360.00)-0.5D0))*360.0D0 ! 0 -> 360
      getJupiterLon=ang
C
C      write(*,*)myname,'Illum:',getJupiterLon
C
      return
      end function getJupiterLon
      real function getMoonEarthDist(JD,crc250,irc)
C
C     returns celestial longtitude of the moon...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real moonPos(6),posCartGeoTd(6)
C
      character*16 myname
      data myname /'getMoonEarthDist'/
      real pa,xa,ca,rr,atan2deg
      external atan2deg
      integer length
      external length
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate Moon-Earth distance
C
      getMoonEarthDist=dsqrt(moonPos(1)*moonPos(1)+
     &     moonPos(2)*moonPos(2)+
     &     moonPos(3)*moonPos(3))
C
      return
      end function getMoonEarthDist
      real function getLunarTime(JD,loclat,loclon,lochgt,crc250,irc)
C
C     returns "polar angle" between north pole and earth-moon vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real moonPos(6)
      real moonCart(3)
      real moonPolar(3)
      real rxy
      real rMoon
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*16 myname
      data myname /'getLunarTime'/
      real ang,ang1,ang2,moonAng,atan2deg
      external atan2deg
      real diffcycle
      external diffcycle
      integer length
      external length
      real a,f
      parameter (a=6378.155 D0,f=1.D0/298.257 D0)
C
      rMoon     = 682500.0D0
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,moonPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      phid=loclat*RAD           ! GEODETIC LATITUDE
      xle=loclon*RAD            ! EAST LONGITUDE OF THE STATION
      xh=lochgt/1000.0D0        ! ALTITUDE OF THE STATION ABOVE THE SURFACE OF THE REFERENCE ELLIPSOID (KM)
C      A = 6378.155 D0 ! mean radius of equator
C      F = 1.D0/298.257 D0 ! flattening coefficient
      call iau_GD2GCE ( A, F, xle, phid, xh, r, IRC )
C
C     consider angles in equator plane
C
      ang1=atan2deg(r(2),r(1))
      ang2=atan2deg(moonPos(2),moonPos(1))

      ang=diffCycle(ang1,ang2,360D0)
      if (ang.gt.180.0D0) ang=ang-360.0D0
      getLunarTime=12.0D0 + 24.0D0*ang/360.0D0
C
      return
      end function getLunarTime
      real function getMarsLon(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real marsPos(6)
      real atan2deg,marsAng,sunAng,ang
      external atan2deg
      integer length
      external length
C
      character*16 myname
      data myname /'getMarsLon'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMarsMJ(marsPos,crc250,irc) ! retrieve Mars geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMarsMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(marsPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,marsPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,marsPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate angle between sun and mars pos as seen from the earth
C
      marsAng=atan2deg(marsPos(2),marsPos(1))
      sunAng=atan2deg(sunPos(2),sunPos(1))
      ang=(marsAng-sunAng)
      ang=ang-real(nint((ang/360.00)-0.5D0))*360.0D0 ! 0 -> 360
      getMarsLon=ang
C
C      write(*,*)myname,'Illum:',getMarsLon
C
      return
      end function getMarsLon
      real function getMercElong(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real mercPos(6)
      real earthPos(6)
      real atan2deg,mercAng,earthAng,sunAng,elong,ang,getAngle
      external atan2deg,getAngle
      integer length
      external length
C
      character*12 myname
      data myname /'getMercElong'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMercuryMJ(mercPos,crc250,irc) ! retrieve Mercury geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMercuryMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(mercPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,mercPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,mercPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
C     calcualte mercury elongation as seen from earth
C
      elong=getAngle(mercPos,sunPos)
C
      call newOrigo(mercPos,sunPos) ! shift origo to sun center
C
      earthPos(1)=-sunPos(1)
      earthPos(2)=-sunPos(2)
      earthPos(3)=-sunPos(3)
C     
C     Calculate angle between earth and mercury pos as seen from the sun
C
      mercAng=atan2deg(mercPos(2),mercPos(1))
      earthAng=atan2deg(earthPos(2),earthPos(1))
      ang=(earthAng-mercAng)
C      write(*,*) myname,'Ang before:',ang
      ang=ang-(real(nint((ang/360.00)-0.5D0))*360.0D0) ! 0 -> 360
C      write(*,*) myname,'Ang after: ',ang
C

      if (ang.lt.180.0D0) elong=-elong
C      write(*,'(X,A,A,6(X,F10.3))')myname,'RelAng/elong',
C     &     ang,earthAng-mercAng,
C     &     -real(nint(((earthAng-mercAng)/360.00)-0.5D0))*360.0D0,
C     &     earthAng,mercAng,elong

      getMercElong=elong
C
      return
      end function getMercElong
      real function getMercRelLon(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real mercPos(6)
      real earthPos(6)
      real atan2deg,mercAng,earthAng,sunAng,ang
      external atan2deg
      integer length
      external length
C
      character*16 myname
      data myname /'getMercRelLon'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMercuryMJ(mercPos,crc250,irc) ! retrieve Mercury geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMercuryMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(mercPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,mercPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,mercPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call newOrigo(mercPos,sunPos) ! shift origo to merc center
C
      earthPos(1)=-sunPos(1)
      earthPos(2)=-sunPos(2)
      earthPos(3)=-sunPos(3)
C     
C     Calculate angle between earth and mercury pos as seen from the sun
C
      mercAng=atan2deg(mercPos(2),mercPos(1))
      earthAng=atan2deg(earthPos(2),earthPos(1))
      ang=(earthAng-mercAng)
      ang=ang-real(nint((ang/360.00)-0.5D0))*360.0D0 ! 0 -> 360
      getMercRelLon=ang
C
C      write(*,*)myname,'Illum:',getMercRelLon
C
      return
      end function getMercRelLon
      real function getMercTransit(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real mercPos(6)
      real earthPos(6)
      real atan2deg,acosdeg,getAbs,getDot
      real mercAng,earthAng,sunAng,elong,ang,getAngle
      external atan2deg,getAngle,acosdeg,getAbs,getDot
      real Re,Rs,Rm,ca,tai,alpha,res,iSun(3)
      real fact,ra,da,dam,xpos(3),delta(3),dot,rem,dx
      integer length
      external length
C
      character*16 myname
      data myname /'getMercTransit'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMercuryMJ(mercPos,crc250,irc) ! retrieve Mercury geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMercuryMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(mercPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C     
      Rs   = 682500.0D0         ! sun radius
      Rm   =   2440.0D0         ! mercury radius
      Re   =   6378.0D0         ! earth radius
C
      res=getAbs(sunPos)
      iSun(1)=sunPos(1)/res
      iSun(2)=sunPos(2)/res
      iSun(3)=sunPos(3)/res
C
      ca=(Rs-Re)/res
      alpha=acosdeg(ca)
      tai=ca/dsqrt(1-ca*ca) ! 1/tan(alpha)
C     
      dot=getDot(iSun,mercPos)
      rem=getAbs(mercPos)
C
      delta(1)=mercPos(1)-iSun(1)*dot
      delta(2)=mercPos(2)-iSun(2)*dot
      delta(3)=mercPos(3)-iSun(3)*dot
C
      da=getAbs(delta) ! normal distance from merc to R-earth-sun
C
      delta(1)=delta(1)+iSun(1)*da*tai
      delta(2)=delta(2)+iSun(2)*da*tai
      delta(3)=delta(3)+iSun(3)*da*tai
C
      dam=getAbs(delta) ! distance from merc to R-earth-sun normal on penumbra
C
      xpos(1)=mercPos(1)-delta(1)
      xpos(2)=mercPos(2)-delta(2)
      xpos(3)=mercPos(3)-delta(3)
C
      dx=getAbs(xpos) ! distance from earth to x on R-earth-sun
C
      fact=dx/res ! how far along R-earth-sun is merc normal line on penumbra
C
      ra=fact*rs+(1.0D0-fact)*re ! distance from R-earth-sun to penumbra
C
      getMercTransit = dam-ra  ! penumbra distance
C
C      write(*,*)'GETMERCPENUMBRA:',JD,dam,ra,dam-ra
C
      return
      end function getMercTransit
      real function getMoonAzimuth(JD,loclat,loclon,lochgt,
     &     crc250,irc)
C
C     returns "polar angle" between north pole and earth-moon vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real moonPos(6)
      real moonCart(3)
      real moonPolar(3)
      real rxy
      real rMoon
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*16 myname
      data myname /'getMoonAzimuth'/
      real ang,moonAng,atan2deg,getAngle
      external atan2deg,getAngle
      integer length
      external length
C
      rMoon     = 682500.0D0
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,moonPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,moonPos, ! EARTH FIXED
     &     moonCart,            ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     moonPolar,           ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if

C
      azimuth=moonPolar(3)*DEG
      elevation=moonPolar(2)*DEG
     &     + (rMoon/moonPolar(1))*DEG
C
      getMoonAzimuth=azimuth

C     write(*,*)myname,'Angles:',azimuth,elevation,getMoonAzimuth
      return
      end function getMoonAzimuth
      real function getMoonElevation(JD,loclat,loclon,lochgt,
     &     crc250,irc)
C
C     returns "polar angle" between north pole and earth-moon vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real moonPos(6)
      real moonCart(3)
      real moonPolar(3)
      real rxy
      real rMoon
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*16 myname
      data myname /'getMoonElevation'/
      real ang,moonAng,atan2deg,getAngle
      external atan2deg,getAngle
      integer length
      external length
C
      rMoon     =  1738.0D0
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,moonPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,moonPos, ! EARTH FIXED
     &     moonCart,            ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     moonPolar,           ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if

C
      azimuth=moonPolar(3)*DEG
      elevation=moonPolar(2)*DEG
     &     + (rMoon/moonPolar(1))*DEG
C     
C     Target: lat         gives moon set
C     Target: lat+360.0D0 gives moon rise
C
      getMoonElevation=elevation
C      write(*,*)myname,'Angles:',azimuth,elevation,
C     &     getMoonElevation,moonPolar(2)*DEG
      return
      end function getMoonElevation
C
      real function getMoonIllum(JD,crc250,irc)
C
C     returns moon illumination as seen from earth center...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real moonPos(6)
      real earthPos(6)
      real sunAng,moonAng,ang,acosdeg,ca,rr
      external acosdeg
      integer length
      external length
C
      character*12 myname
      data myname /'getMoonIllum'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call newOrigo(sunPos,moonPos) ! shift origo to moon center
      call normalise(sunPos) ! normalise sun position vector, |R|=1
C
      earthPos(1)=-moonPos(1)
      earthPos(2)=-moonPos(2)
      earthPos(3)=-moonPos(3)
      
      call normalise(earthPos) ! normalise earth position vector, |R|=1
C     
C     Calculate cosine between earth and sun pos as seen from moon
C
      ca=(sunPos(1)*earthPos(1)+
     &     sunPos(2)*earthPos(2)+
     &     sunPos(3)*earthPos(3))
      ang=acosdeg(ca)
      getMoonIllum=0.5D0 * (1.0D0 + ca) * 100.0D0 ! 0->100 (max illum)
C
C      write(*,*)myname,'Illum:',getMoonIllum
C
      return
      end function getMoonIllum
      real function getMoonOcc(JD,crc250,irc)
C
C     returns distance between moon and earth umbra...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real moonPos(6)
      real earthPos(6)
      real atan2deg,acosdeg,sindeg,getAbs,getDot
      real moonAng,earthAng,sunAng,elong,ang,getAngle
      external atan2deg,getAngle,acosdeg,sindeg,getAbs,getDot
      real Re,Rs,Rm,ca,tai,alpha,res,iSun(3)
      real fact,ru,da,xpos(3),delta(3),dot,rem,desx,dx,ddx,h
      real occ,iocc
      real area,pi,d
      parameter (pi=3.1415927D0)
      integer length
      external length
C
      character*16 myname
      data myname /'getMoonOcc'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C     
      Rs   = 682500.0D0         ! sun radius
      Rm   =   1738.0D0         ! moon radius
      Re   =   6378.0D0         ! earth radius
C
C     Earth-sun unity vector
C
      res=getAbs(sunPos)
      iSun(1)=sunPos(1)/res
      iSun(2)=sunPos(2)/res
      iSun(3)=sunPos(3)/res
C
C     umbra angle (90 -> infinity)
C
      ca=(Rs-Re)/res
      alpha=acosdeg(ca)
      tai=ca/dsqrt(1-ca*ca) ! 1/tan(alpha)
C     
      dot=getDot(iSun,moonPos)  ! must be negative...
      rem=getAbs(moonPos)
C
      delta(1)=moonPos(1)-iSun(1)*dot
      delta(2)=moonPos(2)-iSun(2)*dot
      delta(3)=moonPos(3)-iSun(3)*dot
C
      da=getAbs(delta) ! normal distance from moon to R-earth-sun
C
      delta(1)=delta(1)+iSun(1)*da*tai
      delta(2)=delta(2)+iSun(2)*da*tai
      delta(3)=delta(3)+iSun(3)*da*tai
C
      d=getAbs(delta)           ! distance from moon centre to umbra centre
C
      xpos(1)=moonPos(1)-delta(1)
      xpos(2)=moonPos(2)-delta(2)
      xpos(3)=moonPos(3)-delta(3)
C
      dx=getAbs(xpos)           ! distance from earth to x on R-earth-sun
      desx=res*(re/(rs-re))     ! distance from earth to umbra focal point
C
      ru=re*((desx-dx)/desx)    ! umbra radius
C
C     calculate occulation
C
      if ( d-rm .gt. ru) then  ! moon completely outside umbra
         occ=0.0D0
      else if (d+rm .lt. ru) then ! moon completely within umbra
         occ=100.0D0
      else if (d+ru .lt. rm) then ! umbra completely within moon radius (this can never happen)
         occ=100.0D0*(ru*ru)/(rm*rm)
      else                      ! moon partially within umbra
C     http://mathworld.wolfram.com/Circle-CircleIntersection.html
         Area=+(rm*rm)*acos((d*d+rm*rm-ru*ru)/(2.0*d*rm))
     &        +(ru*ru)*acos((d*d+ru*ru-rm*rm)/(2.0*d*ru))
     &        -0.5D0*dsqrt((-d+rm+ru)*(d+rm-ru)*(d-rm+ru)*(d+rm+ru))
         occ = 100.0D0* Area/(rm*rm*pi)
      end if
C
C      write(*,*)'GETMOONOCC:',JD,occ
C
      getMoonOcc=occ
      return
C
      end function getMoonOcc
      real function getMoonPhase(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real moonPos(6)
      real earthPos(6)
      real sunAng,earthAng,ang
C
      character*12 myname
      data myname /'getMoonPhase'/
      real pa,xa,ca,rr,atan2deg
      external atan2deg
      integer length
      external length
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
C     call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
C     if (irc.ne.0) then
C      lencrc=length(crc250,250,10)
C      crc250=crc250(1:lencrc)//' < '//myname//
C     &     ' > Error return from JPLephEQtoEC.'
C      call chop0(crc250,250)
C     return
C     end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from moonPos.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,moonPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,moonPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call newOrigo(sunPos,moonPos) ! shift origo to moon center

      earthPos(1)=-moonPos(1)
      earthPos(2)=-moonPos(2)
      earthPos(3)=-moonPos(3)
C     
C     Calculate solar celestial longtitude... (X-axis points towards vernal equinox)
C
      sunAng=atan2deg(sunPos(2),sunPos(1))
      earthAng=atan2deg(earthPos(2),earthPos(1))
      ang=(earthAng-sunAng)+180.0D0
      ang=ang-real(nint((ang/360.00)-0.5))*360.0D0 ! 0 -> 360
      getMoonPhase=ang/3.6D0 ! 0->100
C
      return
      end function getMoonPhase
      real function getMoonUpperElevation(JD,
     &     loclat,loclon,lochgt,crc250,irc)
C
C     returns "polar angle" between north pole and earth-moon vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real moonPos(6)
      real moonCart(3)
      real moonPolar(3)
      real rxy
      real rMoon
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*24 myname
      data myname /'getMoonUpperElevation'/
      real ang,moonAng,atan2deg,getAngle
      external atan2deg,getAngle
      real diffcycle
      external diffcycle
      integer length
      external length
C
      rMoon     = 1738.0D0
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,moonPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,moonPos, ! EARTH FIXED
     &     moonCart,            ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     moonPolar,           ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if

C
      azimuth=moonPolar(3)*DEG
      elevation=moonPolar(2)*DEG
     &     + (rMoon/moonPolar(1))*DEG
C     
C     Target: lat         gives moon set
C     Target: lat+360.0D0 gives moon rise
C
      if (abs(diffCycle(azimuth,90.0D0,360.0D0)).lt.90.0D0) then ! 12pm -> 12am (morning)
         getMoonUpperElevation=elevation
      else                      ! evening
         getMoonUpperElevation=elevation+360.0D0
      end if
C      write(*,*)myname,'Angles:',azimuth,elevation,
C     &     getMoonUpperElevation,moonPolar(2)*DEG
C      write(*,*)myname,'******:',azimuth,
C     &     diffCycle(azimuth,90.0D0,360.0D0),
C     &     elevation,getMoonUpperElevation
      return
      end function getMoonUpperElevation

      real function getPenumbra(JD,crc250,irc)
C
C     returns distance between moon and earth penumbra...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real moonPos(6)
      real earthPos(6)
      real atan2deg,acosdeg,getAbs,getDot
      real moonAng,earthAng,sunAng,elong,ang,getAngle
      external atan2deg,getAngle,acosdeg,getAbs,getDot
      real Re,Rs,Rm,ca,tai,alpha,res,iSun(3)
      real fact,ra,da,dam,xpos(3),delta(3),dot,rem,desx,dx
      integer length
      external length
C
      character*16 myname
      data myname /'getPenumbra'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C     
      Rs   = 682500.0D0         ! sun radius
      Rm   =   1738.0D0         ! moon radius
      Re   =   6378.0D0         ! earth radius
C
C     Earth-sun unity vector
C
      res=getAbs(sunPos)
      iSun(1)=sunPos(1)/res
      iSun(2)=sunPos(2)/res
      iSun(3)=sunPos(3)/res
C
C     penumbra angle (90 -> infinity)
C
      ca=(Rs+Re)/res
      alpha=acosdeg(ca)
      tai=ca/dsqrt(1-ca*ca) ! 1/tan(alpha)
C     
      dot=getDot(iSun,moonPos)
      rem=getAbs(moonPos)
C
      delta(1)=moonPos(1)-iSun(1)*dot
      delta(2)=moonPos(2)-iSun(2)*dot
      delta(3)=moonPos(3)-iSun(3)*dot
C
      da=getAbs(delta) ! normal distance from moon to R-earth-sun
C
      delta(1)=delta(1)-iSun(1)*da*tai
      delta(2)=delta(2)-iSun(2)*da*tai
      delta(3)=delta(3)-iSun(3)*da*tai
C
      dam=getAbs(delta) ! distance from moon to R-earth-sun normal on penumbra
C
      xpos(1)=moonPos(1)-delta(1)
      xpos(2)=moonPos(2)-delta(2)
      xpos(3)=moonPos(3)-delta(3)
C
      dx=getAbs(xpos)           ! distance from earth to x on R-earth-sun
      desx=res*(re/(re+rs))     ! distance from earth to penumbra focal point
C
      ra=re*((dx+desx)/(desx))
C
      getPenumbra = dam-ra  ! penumbra distance
C
C      write(*,*)'GETPENUMBRA:',JD,dam,ra,dam-ra
C
      return
      end function getPenumbra
      real function getPolarSunAngle(JD,loclat,loclon,lochgt,
     &     crc250,irc)
C
C     returns "polar angle" between north pole and earth-sun vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real axis(6)
      real rSun,rEarth,dist
C
      character*20 myname
      data myname /'getPolarSunAngle'/
      real ang0,ang,sunAng,atan2deg,getAngle,getAbs
      external atan2deg,getAngle,getAbs
      integer length
      external length
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate solar celestial latitude... (Z-axis points towards north pole)
C
      axis(1)=0.0D0
      axis(2)=0.0D0
      axis(3)=1.0D0
C
      ang0=getAngle(sunPos,axis) ! 0 -> 180.0D0
C
C     correct for upper sun limb (assuming that Sun disc has same size locally as from GC)
C     
      rSun     = 682500.0D0
      rEarth   =   6378.0D0 
      dist=getAbs(sunPos)
C
      if (loclat.gt.0.0D0) then
         ang=ang0 - ((rSun-rEarth)/dist)*DEG
      else
         ang=ang0 + ((rSun-rEarth)/dist)*DEG
      end if
C
C     Target=
C     lat:       gives polar night stop
C     -lat:      gives polar day start
C     180-lat:   gives polar day stop
C     -180+lat:  gives polar night start
C
      getPolarSunAngle=ang
C
      return
      end function getPolarSunAngle
      real function getPolarSunAngleX(JD,loclat,loclon,lochgt,
     &     crc250,irc)
C
C     returns "polar angle" between north pole and earth-sun vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real axis(6)
      real rSun,rEarth,dist
C
      character*20 myname
      data myname /'getPolarSunAngleX'/
      real ang0,ang,sunAng,atan2deg,getAngle,getAbs
      external atan2deg,getAngle,getAbs
      integer length
      external length
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate solar celestial latitude... (Z-axis points towards north pole)
C
      axis(1)=0.0D0
      axis(2)=0.0D0
      axis(3)=1.0D0
C
      ang0=getAngle(sunPos,axis) ! 0 -> 180.0D0
C
C     correct for upper sun limb (assuming that Sun disc has same size locally as from GC)
C     
      rSun     = 682500.0D0
      rEarth   =   6378.0D0 
      dist=getAbs(sunPos)
C
      if (loclat.gt.0.0D0) then
         ang=ang0 - ((rSun-rEarth)/dist)*DEG
      else
         ang=ang0 + ((rSun-rEarth)/dist)*DEG
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
      sunAng=atan2deg(sunPos(2),sunPos(1))-90.0D0 ! 0: sunPos points to south pole
      sunAng=sunAng-real(nint((sunAng/360.00)))*360.0D0 ! -180 -> 180
C
      if (sunAng.lt.0.0D0) then ! autumn half of the year (loclat+90.0D0)
         ang=-ang
      end if
C
C     Target=
C     lat:       gives polar night stop
C     -lat:      gives polar day start
C     180-lat:   gives polar day stop
C     -180+lat:  gives polar night start
C
      getPolarSunAngleX=ang
C
      return
      end function getPolarSunAngleX
      real function getPolarMoonAngle(JD,loclat,loclon,lochgt,
     &     crc250,irc)
C
C     returns "polar angle" between north pole and earth-moon vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real moonPos(6)
      real axis(6)
      real rMoon,rEarth,dist
C
      character*20 myname
      data myname /'getPolarMoonAngle'/
      real ang0,ang,moonAng,atan2deg,getAngle,getAbs
      external atan2deg,getAngle,getAbs
      integer length
      external length
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C
      real moonCart(3)
      real moonPolar(3)
      real rxy
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,moonPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,moonPos, ! EARTH FIXED
     &     moonCart,             ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     moonPolar,            ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if

C
      azimuth=moonPolar(3)*DEG
      elevation=moonPolar(2)*DEG
     &     + ((rMoon-rEarth)/moonPolar(1))*DEG
C     
C      write(*,*)'AngleBB:',r(1),r(2),r(3),
C     &     90.0-(elevation+(34.0/60.0))
C     
C     Calculate lunar celestial latitude... (Z-axis points towards north pole)
C
      axis(1)=0.0D0
      axis(2)=0.0D0
      axis(3)=1.0D0
C
      ang0=getAngle(moonPos,axis) ! 0 -> 180.0D0
      write(*,*)'Moon-north-pole:',ang0
C
C     correct for upper moon limb (assuming that Moon disc has same size locally as from GC)
C     
      rMoon    =   1738.1D0
      rEarth   =   6378.0D0 
      dist=getAbs(moonPos)
C
      if (loclat.gt.0.0D0) then
         ang=ang0 - ((rMoon-rEarth)/dist)*DEG
      else
         ang=ang0 + ((rMoon-rEarth)/dist)*DEG
      end if
C      write(*,*)'Moon-north-pole (corrected):',ang
C
C     Target=
C     lat:       gives polar night stop
C     -lat:      gives polar day start
C     180-lat:   gives polar day stop
C     -180+lat:  gives polar night start
C
      getPolarMoonAngle=ang
C
      return
      end function getPolarMoonAngle
      real function getPolarMoonAngleX(JD,loclat,loclon,lochgt,
     &     crc250,irc)
C     
C     returns "polar angle" between north pole and earth-moon vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C     
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real moonPos(6),moonPos2(6),dtraw
      parameter (dtraw=1.0D0) ! brute force derivation
      real axis(6)
C
      character*20 myname
      data myname /'getPolarMoonAngleX'/
      real dist,rEarth,rMoon
      real ang0,ang,moonAng,atan2deg,getAngle,getAbs
      external atan2deg,getAngle,getAbs
      integer length
      external length
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      moonPos2(1)=moonPos(1)+moonPos(4)*dtraw
      moonPos2(2)=moonPos(2)+moonPos(5)*dtraw
      moonPos2(3)=moonPos(3)+moonPos(6)*dtraw
      call JPLephMJtoTD(JD,moonPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
      call JPLephMJtoTD(JD,moonPos2,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
      moonPos(4)=(moonPos2(1)-moonPos(1))/dtraw
      moonPos(5)=(moonPos2(2)-moonPos(2))/dtraw
      moonPos(6)=(moonPos2(3)-moonPos(3))/dtraw
C     
C     Calculate solar celestial longtitude... (X-axis points towards vernal equinox)
C
      axis(1)=0.0D0
      axis(2)=0.0D0
      axis(3)=1.0D0
C
      ang0=getAngle(moonPos,axis) ! 0 -> 180.0D0
C
C     correct for upper sun limb (assuming that Sun disc has same size locally as from GC)
C     
      rMoon    =   1736.1D0
      rEarth   =   6378.0D0 
      dist=getAbs(moonPos)
C
      write(*,*)'PolarMoonAngle:',ang0,moonPos
C
      if (locLat.gt.0.0D0) then
         ang=ang0 - ((rMoon-rEarth)/dist)*DEG
      else
         ang=ang0 + ((rMoon-rEarth)/dist)*DEG
      end if
C
      call JPLephEQtoORB(JD,moonPos,crc250,irc) ! convert from equator orbit coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
      moonAng=atan2deg(moonPos(2),moonPos(1))-90.0D0 ! 0: moonPos points to south pole
      moonAng=moonAng-real(nint((moonAng/360.00)))*360.0D0 ! -180 -> 180
C
      if (moonAng.lt.0.0D0) then ! autumn half of the year (lat+90.0D0)
         ang=-ang
      end if

C      write(*,*)'POLARMOONANGLE:',moonAng,moonPos

C
C     Target=
C     lat:       gives polar night stop
C     -lat:      gives polar day start
C     180-lat:   gives polar day stop
C     -180+lat:  gives polar night start
C
      getPolarMoonAngleX=ang
C
      return
      end function getPolarMoonAngleX
      real function getSaturnLon(JD,crc250,irc)
C
C     returns celestial longtitude of the Saturn...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real saturnPos(6)
      real atan2deg,saturnAng,sunAng,ang
      external atan2deg
      integer length
      external length
C
      character*16 myname
      data myname /'getSaturnLon'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSaturnMJ(saturnPos,crc250,irc) ! retrieve Saturn geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSaturnMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(saturnPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,saturnPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,saturnPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate angle between sun and saturn pos as seen from the earth
C
      saturnAng=atan2deg(saturnPos(2),saturnPos(1))
      sunAng=atan2deg(sunPos(2),sunPos(1))
      ang=(saturnAng-sunAng)
      ang=ang-real(nint((ang/360.00)-0.5D0))*360.0D0 ! 0 -> 360
      getSaturnLon=ang
C
C      write(*,*)myname,'Illum:',getSaturnLon
C
      return
      end function getSaturnLon
      real function getSolarTime(JD,loclat,loclon,lochgt,crc250,irc)
C
C     returns "polar angle" between north pole and earth-sun vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real sunCart(3)
      real sunPolar(3)
      real rxy
      real rSun
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*16 myname
      data myname /'getSolarTime'/
      real ang,ang1,ang2,sunAng,atan2deg
      external atan2deg
      real diffcycle
      external diffcycle
      integer length
      external length
      real a,f
      parameter (a=6378.155 D0,f=1.D0/298.257 D0)
C
      rSun     = 682500.0D0
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,sunPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      phid=loclat*RAD           ! GEODETIC LATITUDE
      xle=loclon*RAD            ! EAST LONGITUDE OF THE STATION
      xh=lochgt/1000.0D0        ! ALTITUDE OF THE STATION ABOVE THE SURFACE OF THE REFERENCE ELLIPSOID (KM)
C      A = 6378.155 D0 ! mean radius of equator
C      F = 1.D0/298.257 D0 ! flattening coefficient
      call iau_GD2GCE ( A, F, xle, phid, xh, r, IRC )

c$$      call geocor(phid,xle,xh,r,an) ! defined as real*8
C
C     consider angles in equator plane
C
      ang1=atan2deg(r(2),r(1))
      ang2=atan2deg(sunPos(2),sunPos(1))

      ang=diffCycle(ang1,ang2,360D0)
      if (ang.gt.180.0D0) ang=ang-360.0D0
      getSolarTime=12.0D0 + 24.0D0*ang/360.0D0
C
      return
      end function getSolarTime
C
      real function getSunAzimuth(JD,loclat,loclon,lochgt,crc250,irc)
C
C     returns "polar angle" between north pole and earth-sun vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real sunCart(3)
      real sunPolar(3)
      real rxy
      real rSun
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*16 myname
      data myname /'getSunAzimuth'/
      real ang,sunAng,atan2deg,getAngle
      external atan2deg,getAngle
      integer length
      external length
C
      rSun     = 682500.0D0
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,sunPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,sunPos, ! EARTH FIXED
     &     sunCart,             ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     sunPolar,            ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if

C
      azimuth=sunPolar(3)*DEG
      elevation=sunPolar(2)*DEG
     &     + (rSun/sunPolar(1))*DEG
C
      getSunAzimuth=azimuth

C      write(*,*)myname,'Angles:',azimuth,elevation,getSunAzimuth
      return
      end function getSunAzimuth
C
      real function getSunCelLon(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6),posCartGeoTd(6),posCartGeoTdEc(6)
C
      character*12 myname
      data myname /'getSunCelLon'/
      real pa,xa,ca,rr,atan2deg
      external atan2deg
      integer length
      external length
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      getSunCelLon=atan2deg(sunPos(2),sunPos(1))
C      rr=dsqrt(sunPos(1)*sunPos(1)+
C     &     sunPos(2)*sunPos(2)+
C     &     sunPos(3)*sunPos(3))
C      write(*,'(X,A,3(X,F15.10))')myname,
C     &     sunPos(1)/rr,
C     &     sunPos(2)/rr,
C     &     sunPos(3)/rr

      return
      end function getSunCelLon
      real function getSunEarthDist(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6),posCartGeoTd(6)
C
      character*16 myname
      data myname /'getSunEarthDist'/
      real pa,xa,ca,rr,atan2deg
      external atan2deg
      integer length
      external length
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate Sun-Earth distance
C
      getSunEarthDist=dsqrt(sunPos(1)*sunPos(1)+
     &     sunPos(2)*sunPos(2)+
     &     sunPos(3)*sunPos(3))
C
      return
      end function getSunEarthDist
      real function getSunEclipse(JD,loclat,loclon,lochgt,crc250,irc)
C
C     returns distance between moon and Sun cone...
C
      implicit none
      real loclat,loclon,lochgt
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6)
      real moonPos(6)
      real earthPos(6)
      real atan2deg,acosdeg,sindeg,getAbs,getDot
      real moonAng,earthAng,sunAng,elong,ang,getAngle
      external atan2deg,getAngle,acosdeg,sindeg,getAbs,getDot
      real Re,Rs,Rm,ca,tai,alpha,res,iSun(3)
      real fact,ru,da,xpos(3),delta(3),dot,rem,desx,dx,ddx,h
      real occ,iocc
      real area,d
      common /CgetSunEclipse/occ
C
      real sunCart(3),moonCart(3)
      real sunPolar(3),moonPolar(3)
      real rxy
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
      integer length
      external length
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*16 myname
      data myname /'getSunEclipse'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,sunPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,sunPos, ! EARTH FIXED
     &     sunCart,             ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     sunPolar,            ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if

C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,moonPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,moonPos, ! EARTH FIXED
     &     moonCart,            ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     moonPolar,           ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if
C     
C     
      Rs   = 682500.0D0         ! sun radius
      Rm   =   1738.0D0         ! moon radius
      Re   =   6378.0D0         ! earth radius
C
      res=getAbs(sunCart)        ! distance to Sun
      rem=getAbs(moonCart)       ! distance to Moon
C
      iSun(1)=sunCart(1)/res
      iSun(2)=sunCart(2)/res
      iSun(3)=sunCart(3)/res
C
C     cone angle (90 -> infinity)
C
      ca=(Rs/res)
      alpha=acosdeg(ca)
      tai=ca/dsqrt(1-ca*ca) ! 1/tan(alpha)
C     
      dot=getDot(iSun,moonCart)  ! must be positive...
C
      if (dot.lt.0.0D0) then
         occ=0.0D0
         getSunEclipse = Rs+Rm  ! large cone distance
         return
      end if
C
      delta(1)=moonCart(1)-iSun(1)*dot
      delta(2)=moonCart(2)-iSun(2)*dot
      delta(3)=moonCart(3)-iSun(3)*dot
C
      da=getAbs(delta) ! normal distance from moon to R-earth-sun
C
      delta(1)=delta(1)-iSun(1)*da*tai
      delta(2)=delta(2)-iSun(2)*da*tai
      delta(3)=delta(3)-iSun(3)*da*tai
C
      d=getAbs(delta)           ! distance from moon centre to cone centre
C
      xpos(1)=moonCart(1)-delta(1)
      xpos(2)=moonCart(2)-delta(2)
      xpos(3)=moonCart(3)-delta(3)
C
      dx=getAbs(xpos)           ! distance from earth to x on R-earth-sun
C
      ru=(dx/res)*Rs            ! cone radius at moon distance
C
C     calculate occulation
C
      if ( d-rm .gt. ru) then  ! moon completely outside cone
         occ=0.0D0
      else if (d+rm .lt. ru) then ! moon completely within cone
         occ=100.0D0*(rm*rm)/(ru*ru)
      else if (d+ru .lt. rm) then ! cone completely within moon radius
         occ=100.0D0
      else                      ! moon partially within cone
C     http://mathworld.wolfram.com/Circle-CircleIntersection.html
         Area=+(rm*rm)*acos((d*d+rm*rm-ru*ru)/(2.0*d*rm))
     &        +(ru*ru)*acos((d*d+ru*ru-rm*rm)/(2.0*d*ru))
     &        -0.5D0*dsqrt((-d+rm+ru)*(d+rm-ru)*(d-rm+ru)*(d+rm+ru))
         occ = 100.0D0* Area/(ru*ru*pi)
C         write(*,*)myname,'Occ:',occ

      end if
C
C     get distance between SunEclipse and moon center
C
      getSunEclipse = d-ru      ! cone distance
C
C     write(*,*)'GETCONE:',JD,d,ru,d-ru,desx,dx
C
      return
      end function getSunEclipse
C
      real function getSunEclipseOcc()
      implicit none
      real occ
      common /CgetSunEclipse/occ
C      write(*,*)myname,'Get Occ:',occ
C     Occulation (in %)
      getSunEclipseOcc=occ
      return
      end function getSunEclipseOcc
C
      real function getSunElevation(JD,loclat,loclon,lochgt,
     &     crc250,irc)
C
C     returns "polar angle" between north pole and earth-sun vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real sunCart(3)
      real sunPolar(3)
      real rxy
      real rSun
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*18 myname
      data myname /'getSunElevation'/
      real ang,sunAng,atan2deg,getAngle
      external atan2deg,getAngle
      real diffcycle
      external diffcycle
      integer length
      external length
C
      rSun     = 682500.0D0
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,sunPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,sunPos, ! EARTH FIXED
     &     sunCart,             ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     sunPolar,            ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if

C
      azimuth=sunPolar(3)*DEG
      elevation=sunPolar(2)*DEG
C     &     + (rSun/sunPolar(1))*DEG
C     
C     Target: lat         gives sun set
C     Target: lat+360.0D0 gives sun rise
C
      if (abs(diffCycle(azimuth,90.0D0,360.0D0)).lt.90.0D0) then ! 12pm -> 12am (morning)
         getSunElevation=elevation
      else                      ! evening
         getSunElevation=elevation
      end if
C     write(*,*)myname,'Angles:',azimuth,elevation,getSunElevation
      return
      end function getSunElevation
C
      real function getSunOcc(JD,loclat,loclon,lochgt,crc250,irc)
C
C     returns distance between moon and Sun cone...
C
      implicit none
      real loclat,loclon,lochgt
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real moonPos(6)
      real earthPos(6)
      real atan2deg,acosdeg,sindeg,getAbs,getDot
      real moonAng,earthAng,sunAng,elong,ang,getAngle
      external atan2deg,getAngle,acosdeg,sindeg,getAbs,getDot
      real Re,Rs,Rm,ca,tai,alpha,res,iSun(3)
      real fact,ru,da,xpos(3),delta(3),dot,rem,desx,dx,ddx,h
      real occ,iocc
      real area,d
C
      real getSunOccOcc
C
      real sunCart(3),moonCart(3)
      real sunPolar(3),moonPolar(3)
      real rxy
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
      integer length
      external length
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*16 myname
      data myname /'getSunOcc'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,sunPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,sunPos, ! EARTH FIXED
     &     sunCart,             ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     sunPolar,            ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if

C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,moonPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,moonPos, ! EARTH FIXED
     &     moonCart,            ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     moonPolar,           ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if
C     
      Rs   = 682500.0D0         ! sun radius
      Rm   =   1738.0D0         ! moon radius
      Re   =   6378.0D0         ! earth radius
C
      res=getAbs(sunCart)        ! distance to Sun
      rem=getAbs(moonCart)       ! distance to Moon
C
      iSun(1)=sunCart(1)/res
      iSun(2)=sunCart(2)/res
      iSun(3)=sunCart(3)/res
C
C     cone angle (90 -> infinity)
C
      ca=(Rs/res)
      alpha=acosdeg(ca)
      tai=ca/dsqrt(1-ca*ca) ! 1/tan(alpha)
C     
      dot=getDot(iSun,moonCart)  ! must be negative...
C
      delta(1)=moonCart(1)-iSun(1)*dot
      delta(2)=moonCart(2)-iSun(2)*dot
      delta(3)=moonCart(3)-iSun(3)*dot
C
      da=getAbs(delta) ! normal distance from moon to R-earth-sun
C
      delta(1)=delta(1)-iSun(1)*da*tai
      delta(2)=delta(2)-iSun(2)*da*tai
      delta(3)=delta(3)-iSun(3)*da*tai
C
      d=getAbs(delta)           ! distance from moon centre to cone centre
C
      xpos(1)=moonCart(1)-delta(1)
      xpos(2)=moonCart(2)-delta(2)
      xpos(3)=moonCart(3)-delta(3)
C
      dx=getAbs(xpos)           ! distance from earth to x on R-earth-sun
C
      ru=(dx/res)*Rs            ! cone radius at moon distance
C
C     calculate occulation
C
      if ( d-rm .gt. ru) then  ! moon completely outside cone
         occ=0.0D0
      else if (d+rm .lt. ru) then ! moon completely within cone
         occ=100.0D0*(rm*rm)/(ru*ru)
      else if (d+ru .lt. rm) then ! cone completely within moon radius
         occ=100.0D0
      else                      ! moon partially within cone
C     http://mathworld.wolfram.com/Circle-CircleIntersection.html
         Area=+(rm*rm)*acos((d*d+rm*rm-ru*ru)/(2.0*d*rm))
     &        +(ru*ru)*acos((d*d+ru*ru-rm*rm)/(2.0*d*ru))
     &        -0.5D0*dsqrt((-d+rm+ru)*(d+rm-ru)*(d-rm+ru)*(d+rm+ru))
         occ = 100.0D0* Area/(ru*ru*pi)
      end if
C
C     get distance between SunEclipse and moon center
C
      getSunOcc = occ           ! occultation
C
C      write(*,*)'OCC:',JD,occ,d
C
      return
C
      end function getSunOcc
      real function getSunRA(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real sunPos(6),posCartGeoTd(6)
C
      character*12 myname
      data myname /'getSunRA'/
      real pa,xa,ca,rr,atan2deg
      external atan2deg
      integer length
      external length
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate solar Right Ascension... (X-axis points towards vernal equinox)
C
      getSunRA=atan2deg(sunPos(2),sunPos(1))
C      rr=dsqrt(sunPos(1)*sunPos(1)+
C     &     sunPos(2)*sunPos(2)+
C     &     sunPos(3)*sunPos(3))
C      write(*,'(X,A,3(X,F15.10))')'GETSUNRA:',
C     &     sunPos(1)/rr,
C     &     sunPos(2)/rr,
C     &     sunPos(3)/rr

      return
      end function getSunRA
      real function getMoonRA(JD,crc250,irc)
C
C     returns celestial longtitude of the moon...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
      real moonPos(6),posCartGeoTd(6)
C
      character*12 myname
      data myname /'getMoonRA'/
      real pa,xa,ca,rr,atan2deg
      external atan2deg
      integer length
      external length
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,moonPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoORB(JD,moonPos,crc250,irc) ! convert from equator orbit coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C     
C     Calculate solar "Right Ascension"... (X-axis points towards lunar vernal equinox)
C
      getMoonRA=atan2deg(moonPos(2),moonPos(1))
C      rr=dsqrt(moonPos(1)*moonPos(1)+
C     &     moonPos(2)*moonPos(2)+
C     &     moonPos(3)*moonPos(3))
C      write(*,'(X,A,3(X,F15.10))')'GETMOONRA:',
C     &     moonPos(1)/rr,
C     &     moonPos(2)/rr,
C     &     moonPos(3)/rr

      return
      end function getMoonRA
      real function getSunUpperElevation(JD,
     &     loclat,loclon,lochgt,crc250,irc)
C
C     returns "polar angle" between north pole and earth-sun vector.
C     The polar angle is always increasing (with 2 discontinuities), 
C     making it easier to find target values...
C
      implicit none
      real JD
      real loclat
      real loclon
      real lochgt
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real sunCart(3)
      real sunPolar(3)
      real rxy
      real rSun
      real axis(6)
      real phid,xle,xh          ! geodetic lat,lon,altitude above ellipsoide
      real teta,azimuth,elevation
      real*8 r(4), an(7)        ! location in Geocentric Cartesian Earth-Fixed
      integer mode
      parameter (mode=3)        ! only transform pos
C     
      real*8 PI,TWOPI,PIH,DEG,RAD
      COMMON /CBASIC/ PI,TWOPI,PIH,DEG,RAD
C
      character*24 myname
      data myname /'getSunUpperElevation'/
      real ang,sunAng,atan2deg,getAngle
      external atan2deg,getAngle
      real diffcycle
      external diffcycle
      integer length
      external length
C
      rSun     = 682500.0D0
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoEF(JD,sunPos,crc250,irc) ! convert to "Earth Fixed"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C     
      call JPLephEFtoTC(loclat,loclon,lochgt,sunPos, ! EARTH FIXED
     &     sunCart,             ! TOPOCENTRIC (EAST,NORTH,ZENITH) (KM)
     &     sunPolar,            ! RANGE, ELEVATION, AZIMUTH (KM,RAD)
     &     crc250,irc)
      if(irc.ne.0) then
         write(*,*)myname,'Error return from JPLephEFtoTC.',irc
         return
      end if
C
      azimuth=sunPolar(3)*DEG
      elevation=sunPolar(2)*DEG
     &     + (rSun/sunPolar(1))*DEG
C     
C      write(*,*)'AngleAA:',r(1),r(2),r(3),
C     &     90.0-(elevation+(34.0/60.0))
C     
C     Target: lat         gives sun set
C     Target: lat+360.0D0 gives sun rise
C
C      if (abs(diffCycle(azimuth,90.0D0,360.0D0)).lt.90.0D0) then ! 12pm -> 12am (morning)
         getSunUpperElevation=elevation
C      else                      ! evening
C         getSunUpperElevation=elevation+360.0D0
C      end if
C      write(*,*)myname,'******:',azimuth,
C     &     diffCycle(azimuth,90.0D0,360.0D0),
C     &     elevation,getSunUpperElevation
      return
      end function getSunUpperElevation
C
      real function getUmbra(JD,crc250,irc)
C
C     returns distance between moon and earth umbra...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real moonPos(6)
      real earthPos(6)
      real atan2deg,acosdeg,sindeg,getAbs,getDot
      real moonAng,earthAng,sunAng,elong,ang,getAngle
      external atan2deg,getAngle,acosdeg,sindeg,getAbs,getDot
      real Re,Rs,Rm,ca,tai,alpha,res,iSun(3)
      real fact,ru,da,xpos(3),delta(3),dot,rem,desx,dx,ddx,h
      real occ,iocc
      real area,pi,d
      parameter (pi=3.1415927D0)
      integer length
      external length
C
      common /CgetUmbra/occ
C
      character*16 myname
      data myname /'getUmbra'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMoonMJ(moonPos,crc250,irc) ! retrieve Moon geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMoonMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(moonPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C     
      Rs   = 682500.0D0         ! sun radius
      Rm   =   1738.0D0         ! moon radius
      Re   =   6378.0D0         ! earth radius
C
C     Earth-sun unity vector
C
      res=getAbs(sunPos)
      iSun(1)=sunPos(1)/res
      iSun(2)=sunPos(2)/res
      iSun(3)=sunPos(3)/res
C
C     umbra angle (90 -> infinity)
C
      ca=(Rs-Re)/res
      alpha=acosdeg(ca)
      tai=ca/dsqrt(1-ca*ca) ! 1/tan(alpha)
C     
      dot=getDot(iSun,moonPos)  ! must be negative...
      rem=getAbs(moonPos)
C
      delta(1)=moonPos(1)-iSun(1)*dot
      delta(2)=moonPos(2)-iSun(2)*dot
      delta(3)=moonPos(3)-iSun(3)*dot
C
      da=getAbs(delta) ! normal distance from moon to R-earth-sun
C
      delta(1)=delta(1)+iSun(1)*da*tai
      delta(2)=delta(2)+iSun(2)*da*tai
      delta(3)=delta(3)+iSun(3)*da*tai
C
      d=getAbs(delta)           ! distance from moon centre to umbra centre
C
      xpos(1)=moonPos(1)-delta(1)
      xpos(2)=moonPos(2)-delta(2)
      xpos(3)=moonPos(3)-delta(3)
C
      dx=getAbs(xpos)           ! distance from earth to x on R-earth-sun
      desx=res*(re/(rs-re))     ! distance from earth to umbra focal point
C
      ru=re*((desx-dx)/desx)    ! umbra radius
C
C     calculate occulation
C
      if ( d-rm .gt. ru) then  ! moon completely outside umbra
         occ=0.0D0
      else if (d+rm .lt. ru) then ! moon completely within umbra
         occ=100.0D0
      else if (d+ru .lt. rm) then ! umbra completely within moon radius (this can never happen)
         occ=100.0D0*(ru*ru)/(rm*rm)
      else                      ! moon partially within umbra
C     http://mathworld.wolfram.com/Circle-CircleIntersection.html
         Area=+(rm*rm)*acos((d*d+rm*rm-ru*ru)/(2.0*d*rm))
     &        +(ru*ru)*acos((d*d+ru*ru-rm*rm)/(2.0*d*ru))
     &        -0.5D0*dsqrt((-d+rm+ru)*(d+rm-ru)*(d-rm+ru)*(d+rm+ru))
         occ = 100.0D0* Area/(rm*rm*pi)
      end if
C
C     get distance between Umbra and moon center
C
      if (dx+rm.gt.desx) then   ! moon behind umbra (this should never happen)
         getUmbra = 2.0*Rm      ! moon can never be inside umbra
      else if (dx.gt.desx) then ! umbra focal point inside moon (this should never happen)
         ddx=(dx-desx)*sindeg(alpha)
         h=dsqrt( rm*rm - (ddx)*(ddx) ) ! radius of disk at umbra focal point
         getUmbra = d-ru + (Rm-h)
      else
         getUmbra = d-ru      ! umbra distance
      end if
C
C       write(*,*)'GETUMBRA:',JD,d,ru,d-ru,desx,dx
C
      return
      end function getUmbra
C
C     Occulation (in %)
C
      real function getUmbraOcc()
      real occ
      common /CgetUmbra/occ
      getUmbraOcc=occ
      return
      end function getUmbraOcc
C
      real function getVenusElong(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real venusPos(6)
      real earthPos(6)
      real atan2deg,venusAng,earthAng,sunAng,elong,getAngle,ang
      external atan2deg,getAngle
      integer length
      external length
C
      character*16 myname
      data myname /'getVenusElong'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephVenusMJ(venusPos,crc250,irc) ! retrieve Venus geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephVenusMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(venusPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,venusPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,venusPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
C     calcualte venus elongation as seen from earth
C
      elong=getAngle(venusPos,sunPos)
C
      call newOrigo(venusPos,sunPos) ! shift origo to sun center
C
      earthPos(1)=-sunPos(1)
      earthPos(2)=-sunPos(2)
      earthPos(3)=-sunPos(3)
C     
C     Calculate angle between earth and venus pos as seen from the sun
C
      venusAng=atan2deg(venusPos(2),venusPos(1))
      earthAng=atan2deg(earthPos(2),earthPos(1))
      ang=(earthAng-venusAng)
C      write(*,*) myname,'Ang before:',ang
      ang=ang-(real(nint((ang/360.00)-0.5D0))*360.0D0) ! 0 -> 360
C      write(*,*) myname,'Ang after: ',ang
C

      if (ang.lt.180.0D0) elong=-elong
C      write(*,'(X,A,A,6(X,F10.3))')myname,'RelAng/elong',
C     &     ang,earthAng-venusAng,
C     &     -real(nint(((earthAng-venusAng)/360.00)-0.5D0))*360.0D0,
C     &     earthAng,venusAng,elong

      getVenusElong=elong
C
      return
      end function getVenusElong
      real function getVenusRelLon(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real venusPos(6)
      real earthPos(6)
      real atan2deg,venusAng,earthAng,sunAng,ang
      external atan2deg
      integer length
      external length
C
      character*16 myname
      data myname /'getVenusRelLon'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,sunPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,sunPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephVenusMJ(venusPos,crc250,irc) ! retrieve Venus geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephVenusMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(venusPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephMJtoTD(JD,venusPos,crc250,irc) ! convert to "True of date"
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephMJtoTD.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephEQtoEC(JD,venusPos,crc250,irc) ! convert from equator to ecliptic coordinates
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephEQtoEC.'
         call chop0(crc250,250)
         return
      end if
C
      call newOrigo(venusPos,sunPos) ! shift origo to venus center
C
      earthPos(1)=-sunPos(1)
      earthPos(2)=-sunPos(2)
      earthPos(3)=-sunPos(3)
C     
C     Calculate angle between earth and venus pos as seen from the sun
C
      venusAng=atan2deg(venusPos(2),venusPos(1))
      earthAng=atan2deg(earthPos(2),earthPos(1))
      ang=(earthAng-venusAng)
      ang=ang-real(nint((ang/360.00)-0.5D0))*360.0D0 ! 0 -> 360
      getVenusRelLon=ang
C
C      write(*,*)myname,'Illum:',getVenusRelLon
C
      return
      end function getVenusRelLon
      real function getVenusTransit(JD,crc250,irc)
C
C     returns celestial longtitude of the sun...
C
      implicit none
      real JD
      character*250 crc250      ! error description
      integer irc
      integer lencrc
C
      real sunPos(6)
      real mercPos(6)
      real earthPos(6)
      real atan2deg,acosdeg,getAbs,getDot
      real mercAng,earthAng,sunAng,elong,ang,getAngle
      external atan2deg,getAngle,acosdeg,getAbs,getDot
      real Re,Rs,Rm,ca,tai,alpha,res,iSun(3)
      real fact,ra,da,dam,xpos(3),delta(3),dot,rem,dx
      integer length
      external length
C
      character*16 myname
      data myname /'getVenusTransit'/
C
      call JPLephRead(JD,crc250,irc) ! load current solar system positions into memory
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephRead.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephSunMJ(sunPos,crc250,irc) ! retrieve Sun geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephSunMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(sunPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephVenusMJ(mercPos,crc250,irc) ! retrieve Venus geocentric mean-of-date JD position
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephVenusMJ.'
         call chop0(crc250,250)
         return
      end if
C
      call JPLephLight(mercPos,crc250,irc) ! correct for light flight time...
      if (irc.ne.0) then
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//' < '//myname//
     &        ' > Error return from JPLephLight.'
         call chop0(crc250,250)
         return
      end if
C     
      Rs   = 682500.0D0         ! sun radius
      Rm   =   6051.0D0         ! venus radius
      Re   =   6378.0D0         ! earth radius
C
      res=getAbs(sunPos)
      iSun(1)=sunPos(1)/res
      iSun(2)=sunPos(2)/res
      iSun(3)=sunPos(3)/res
C
      ca=(Rs-Re)/res
      alpha=acosdeg(ca)
      tai=ca/dsqrt(1-ca*ca) ! 1/tan(alpha)
C     
      dot=getDot(iSun,mercPos)
      rem=getAbs(mercPos)
C
      delta(1)=mercPos(1)-iSun(1)*dot
      delta(2)=mercPos(2)-iSun(2)*dot
      delta(3)=mercPos(3)-iSun(3)*dot
C
      da=getAbs(delta) ! normal distance from merc to R-earth-sun
C
      delta(1)=delta(1)+iSun(1)*da*tai
      delta(2)=delta(2)+iSun(2)*da*tai
      delta(3)=delta(3)+iSun(3)*da*tai
C
      dam=getAbs(delta) ! distance from merc to R-earth-sun normal on penumbra
C
      xpos(1)=mercPos(1)-delta(1)
      xpos(2)=mercPos(2)-delta(2)
      xpos(3)=mercPos(3)-delta(3)
C
      dx=getAbs(xpos) ! distance from earth to x on R-earth-sun
C
      fact=dx/res ! how far along R-earth-sun is merc normal line on penumbra
C
      ra=fact*rs+(1.0D0-fact)*re ! distance from R-earth-sun to penumbra
C
      getVenusTransit = dam-ra  ! penumbra distance
C
C      write(*,*)'GETMERCPENUMBRA:',JD,dam,ra,dam-ra
C
      return
      end function getVenusTransit
C
      real function diffCycle(val,tval,cyc)
      implicit none
      real val
      real tval
      real cyc
C
      if (cyc.lt.0.0D0) then
         diffCycle=(val-tval)
      else
         diffCycle=(val-tval)-real(nint((val-tval)/cyc))*cyc
      end if
C
      return
      end function diffCycle
      real function degtor(x)
      implicit none
      real x
      real pi
      parameter (pi=3.14159265359)
      degtor=x*pi/180.
      end

      real function rtodeg(x)
      implicit none
      real x
      real pi
      parameter (pi=3.14159265359)
      rtodeg=x*180./pi
      end
      
      real function sindeg(x)
      implicit none
      real x,degtor
      sindeg=sin(degtor(x))
      end

      real function cosdeg(x)
      implicit none
      real x,degtor
      cosdeg=cos(degtor(x))
      end

      real function tandeg(x)
      implicit none
      real x,degtor
      tandeg=tan(degtor(x))
      end

      real function asindeg(x)
      implicit none
      real x,rtodeg
      asindeg=rtodeg(asin(x))
      end

      real function acosdeg(x)
      implicit none
      real x,rtodeg
      acosdeg=rtodeg(acos(x))
      end

      real function atandeg(x)
      implicit none
      real x,rtodeg
      atandeg=rtodeg(atan(x))
      end

      real function atan2deg(x,y)
      implicit none
      real x,y,rtodeg
      atan2deg=rtodeg(atan2(x,y))
      end

      subroutine newOrigo(pos,origo)
      real pos(3),origo(3)
      pos(1)=pos(1)-origo(1)
      pos(2)=pos(2)-origo(2)
      pos(3)=pos(3)-origo(3)
      return
      end subroutine newOrigo

      subroutine normalise(pos)
      real pos(3),rr
      rr=dsqrt(pos(1)*pos(1)+
     &     pos(2)*pos(2)+
     &     pos(3)*pos(3))
      pos(1)=pos(1)/rr
      pos(2)=pos(2)/rr
      pos(3)=pos(3)/rr
      return
      end subroutine normalise
