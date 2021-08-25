      SUBROUTINE MASTRO(UNITI,CRC250,IRC)
C     
C     ***********************************************************************
C     +                                                                     *
C     +  UNITI = UNIT NUMBER FOR INPUT FILE                                 *
C     +  IRC=ERROR RETURN CODE (0=OK)                                       *
C     + 
C     +  Test program for astro-library. 
C     +
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE     |                 REASON                | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 15/03/2011 | NEW                                   | *
C     |              |            |                                       | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C     
      IMPLICIT NONE
C     
C     INTERFACE VARIABLES
C     
      INTEGER UNITI
C     
      CHARACTER*12 MYNAME
      DATA MYNAME /'MASTRO'/
C
C     **********************************************************************
C     INTERNAL INTERFACE VARIABLES
C     **********************************************************************
C
C     ....TIME INPUT
      REAL TSTARTJD           ! START TIME IN JULIAN DAY JD (UTC), USE JULIANDAY/DJD TO CONVERT TO DATE
      REAL TENDJD             ! END TIME IN JULIAN DAY JD (UTC), USE JULIANDAY/DJD TO CONVERT TO DATE
      REAL DTMOON, DTSUN,DTSS ! TIME INCREMENT/STEP IN DAYS
      integer meventVal
      parameter (meventVal=5)
      integer eventId           ! requested event id (SEE TABLE BELOW)
      integer neventVal         ! number of event input data (SEE TABLE ABOVE)
      real eventVal(meventVal)  ! event input data (SEE TABLE ABOVE)
C
C     ....LOCATION INPUT
      INTEGER MAXLOC            ! MAXIMUM NUMBER OF LOCATIONS
      PARAMETER (MAXLOC=100)
      INTEGER NLOC              ! ACTUAL NUMBER OF LOCATIONS
      REAL LOCLAT(MAXLOC)       ! GEODETIC LATITUDES OF LOCATIONS (DEGREES)
      REAL LOCLON(MAXLOC)       ! GEODETIC LONGTITUDE OF LOCATIONS (DEGREES)
      REAL LOCHGT(MAXLOC)       ! GEODETIC HEIGHT OF LOCATIONS (NOT USED)
      CHARACTER*100 LOCID100(0:MAXLOC) ! LOCATION NAMES
C
C     ....REPORT OUTPUT
      INTEGER MAXREP            ! MAXIMUM NUMBER OF REPORTS
      PARAMETER (MAXREP=1000)
      INTEGER NREP(0:MAXLOC)    ! ACTUAL NUMBER OF REPORTS, 0: GLOBAL REPORTS
      REAL  REPJD(MAXREP,0:MAXLOC) ! TIME OF REPORT IN JULIAN DAY JD (UTC)
      INTEGER REPID(MAXREP,0:MAXLOC) ! REPORT ID
      REAL REPVAL(MAXREP,0:MAXLOC) ! REPORT VALUE
      CHARACTER*250 REP250(MAXREP,0:MAXLOC) ! REPORT TEXT
      INTEGER SECDEC            ! NUMBER OF SECOND-DECIMALS
      DATA SECDEC/0/
C
      integer ndtg,mdtg
      parameter (mdtg=10)
      character*30 dtg30(mdtg)
C
C     ....INTEGER RETURN CODE (0=NO ERROR DETECTED)
      character*250 crc250
      integer lencrc
      INTEGER IRC
C
C     +  Notes:
C     +  1) Use julianDay/dJD to convert between Julian day JD and date.
C     +  2) Polar day/night is defined independent of longtitude.
C     +     If you for instance want to find out when the "dark time" 
C     +     starts, find the corresponding "polar night start", then
C     +     search backwards for "diurnal sun set".
C     +  3) The time of an eclipse is defined as the time of maximum
C     +     occultation. 
C     +                                                                     *
C
C     **********************************************************************
C     
      INTEGER YY1,MM1,DD1,HH1,MI1
      REAL SEC1
      CHARACTER*16 T16
      real JD
      real target,targetCycle
      integer INREP
      INTEGER IND(MAXREP)
C
      integer iMoon,iSun,iVenus,iMars,iJupiter,iSaturn
      integer iMercury,iEarth,iUranus,iNeptune,iPluto,nobj
      parameter (nobj=20)
      integer npos
      character*7 pos7(nobj)
      real*8 radius(nobj)
      character*100 desc100
C
      character*10 name10(nobj)
      integer maxillum,nillum,maxphase,nphase
      parameter (maxillum=100,maxphase=100)
      real illum(maxillum),phase(maxphase),rin
      real refraction
      parameter (refraction= 34.0D0/60.0D0) ! refraction in degrees ( http://en.wikipedia.org/wiki/Atmospheric_refraction : 34')
C
      INTEGER NRHDR
      PARAMETER (NRHDR=100)
      CHARACTER*250 HDR250(NRHDR),DAT250,DATX250,BUFF250
      character*1000 buff1000
      integer lenbb,nArg
C
      real azi,elv,rng
C
      integer nmocc,maxmocc
      parameter (maxmocc=10)
      logical lmoccmin(0:maxmocc),lmoccmax(0:maxmocc)
      real moccmin(maxmocc),moccmax(maxmocc)
C
      integer nsocc,maxsocc
      parameter (maxsocc=10)
      logical lsoccmin(0:maxsocc),lsoccmax(0:maxsocc)
      real soccmin(maxsocc),soccmax(maxsocc)
C
      integer nsunazi,maxsunazi
      parameter (maxsunazi=360)
      logical lsunazi(0:maxsunazi)
      real sunazi(maxsunazi)
C
      integer nmoonazi,maxmoonazi
      parameter (maxmoonazi=360)
      logical lmoonazi(0:maxmoonazi)
      real moonazi(maxmoonazi)
C
      integer nsolartime,maxsolartime
      parameter (maxsolartime=360)
      logical lsolartime(0:maxsolartime)
      real solartime(maxsolartime)
C
      integer nlunartime,maxlunartime
      parameter (maxlunartime=360)
      logical llunartime(0:maxlunartime)
      real lunartime(maxlunartime)
C
      INTEGER INTOUT,LINE,KODE
      LOGICAL LFLDAT(NRHDR),BDEB,ACTIVE,ENDOFF
      DATA ACTIVE /.FALSE./
C 
      CHARACTER*250 NUKEHEAD
      CHARACTER*1024 JPL1024
      INTEGER LENGTH,FTUNIT,LENS,LENO,LENL,LENR,lenm
      EXTERNAL LENGTH,FTUNIT,NUKEHEAD
      INTEGER II,JJ,OO,LL,RR,RRX,LENF,LEND,LENI,LENH,LENA,LENB,irc2
      logical lall,lnext,lprev
      integer searchCode
C     
C     DEBUG SYSTEM
C     
      BDEB=.false.
      LOCID100(0) = "GLOBAL"
      nphase=0
      nmocc=0
      nsocc=0
      nsunazi=0
      dtmoon=1.0D-1
      dtsun=1.0D-1
      dtss=1.0D0
      lall=.false.
      lprev=.false.
      lnext=.false.
      ndtg=0
C     
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C     
C     0.  INITIALIZE CONSTANTS AND VARIABLES.
C     
      DO II=1,NRHDR
         HDR250(II) = ''
         LFLDAT(II)=.FALSE.
      ENDDO

      HDR250(1) = 'ASTRO V1.0 -V1.0[0]VFMLR&'
      HDR250(2) = 'JPL EPHEMERIDE FILE (DE405) -F[1]&'
      HDR250(3) = 'START TIME UTC (YYYY MM DD HH MI SEC) -START[1]&1'
      HDR250(4) = 'STOP TIME UTC (YYYY MM DD HH MI SEC)'//
     &     ' -STOP[1]&1%1&1&'
      HDR250(5) = 'REPORT ALL EVENTS IN PERIOD -REPORTALL[0]&1%1&1$'
      HDR250(6) = 'REPORT NEXT EVENT -NEXT[0]&1%1&2$'
      HDR250(7) = 'REPORT PREVIOUS EVENT -PREV[0]&1%1&2$'
      HDR250(8) = 'NUMBER OF DECIMALS IN SECONDS OUTPUT -SECDEC[1]'
      HDR250(9) = 'LIST OF LOCATIONS  (LAT LON HGT ID) -LL[*]%2&3&'

      HDR250(10) = 'REPORT LOCAL INITIAL MOON STATE'//
     &     ' -INITMOON[0]%2&3$'
      HDR250(11) = 'REPORT LOCAL TC EF MOON POSITION '//
     &     'AT TIME INCREMENT-MOONPOS[1]%2&3$'
      HDR250(12) = 'REPORT LOCAL INITIAL SUN STATE'//
     &     ' -INITSUN[0]%2&3$'
      HDR250(13) = 'REPORT LOCAL TC EF SUN POSITION '//
     &     'AT TIME INCREMENT -SUNPOS[1]%2&3$'
      HDR250(14) = 'REPORT GC MJD SOLAR SYSTEM '//
     &     'POSITIONS AT TIME INCREMENT -POS[1]%2&3$'

      HDR250(15) = 'DETECT WINTER SOLSTICE -WINTER[0]'
      HDR250(16) = 'DETECT VERNAL EQUINOX -VERNAL[0]'
      HDR250(17) = 'DETECT SUMMER SOLSTICE -SUMMER[0]'
      HDR250(18) = 'DETECT AUTUMNAL EQUINOX -AUTUMNAL[0]'
      HDR250(19) = 'DETECT EARTH IN PERIHELION -PERIHELION[0]'
      HDR250(20) = 'DETECT EARTH IN APHELION -APHELION[0]'

      HDR250(21) = 'DETECT NEW MOON (PHASE=0/100) -MOONNEW[0]'
      HDR250(22) = 'DETECT FIRST QUARTER MOON (PHASE=25) -MOONFIRST[0]'
      HDR250(23) = 'DETECT FULL MOON (PHASE=50) -MOONFULL[0]'
      HDR250(24) = 'DETECT LAST QUARTER MOON (PHASE=75) -MOONLAST[0]'
      HDR250(25) = 'DETECT MOON PHASE (0 TO 100) -MOONPHASE[*]'
      HDR250(26) = 'DETECT MOON ILLUMINATION MINIMUM -MINMOONILLUM[0]'
      HDR250(27) = 'DETECT MOON ILLUMINATION MAXIMUM -MAXMOONILLUM[0]'
      HDR250(28) = 'DETECT MOON ILLUMINATION (0 TO 100) -MOONILLUM[*]'

      HDR250(30) = 'DETECT MERCURY INFERIOR CONJUNCTION -MERINFCON[0]'
      HDR250(31) = 'DETECT MERCURY SUPERIOR CONJUNCTION -MERSUPCON[0]'
      HDR250(32) = 'DETECT MERCURY GREATEST WESTERN ELONGATION'//
     &     ' -MERWESTEL[0]'
      HDR250(33) = 'DETECT MERCURY GREATEST EASTERN ELONGATION'//
     &     ' -MEREASTEL[0]'

      HDR250(34) = 'DETECT VENUS INFERIOR CONJUNCTION -VENINFCON[0]'
      HDR250(35) = 'DETECT VENUS GREATEST WESTERN ELONGATION'//
     &     ' -VENWESTEL[0]'
      HDR250(36) = 'DETECT VENUS SUPERIOR CONJUNCTION -VENSUPCON[0]'
      HDR250(37) = 'DETECT VENUS GREATEST EASTERN ELONGATION'//
     &     ' -VENEASTEL[0]'

      HDR250(38) = 'DETECT MARS CONJUNCTION -MARCON[0]'
      HDR250(39) = 'DETECT MARS WESTERN QUADRATURE -MARWEST[0]'
      HDR250(40) = 'DETECT MARS OPPOSITION -MAROPP[0]'
      HDR250(41) = 'DETECT MARS EASTERN QUADRATURE -MAREAST[0]'

      HDR250(42) = 'DETECT JUPITER CONJUNCTION -JUPCON[0]'
      HDR250(43) = 'DETECT JUPITER WESTERN QUADRATURE -JUPWEST[0]'
      HDR250(44) = 'DETECT JUPITER OPPOSITION -JUPOPP[0]'
      HDR250(45) = 'DETECT JUPITER EASTERN QUADRATURE -JUPEAST[0]'

      HDR250(46) = 'DETECT SATURN CONJUNCTION -SATCON[0]'
      HDR250(47) = 'DETECT SATURN WESTERN QUADRATURE -SATWEST[0]'
      HDR250(48) = 'DETECT SATURN OPPOSITION -SATOPP[0]'
      HDR250(49) = 'DETECT SATURN EASTERN QUADRATURE -SATEAST[0]'

      HDR250(50) = 'DETECT MERCURY TRANSIT'//
     &     ' (ANYWHERE ON EARTH) -MERTRA[0]'
      HDR250(52) = 'DETECT VENUS TRANSIT'//
     &     ' (ANYWHERE ON EARTH) -VENTRA[0]'

      HDR250(55) = 'DETECT LUNAR ECLIPSE (MINOCC MAXOCC) -LUNECLMM[*]'
      HDR250(56) = 'DETECT LUNAR ECLIPSE -LUNECL[0]'
      HDR250(57) = 'REPORT LOCAL VISIBLE MOON'//
     &     ' -VISMOON[0]%2&3$'
      HDR250(58) = 'REPORT LOCAL VISIBLE SUN'//
     &     ' -VISSUN[0]%2&3$'

      HDR250(60) = 'DETECT LOCAL DIURNAL SUN RISE -SUNRISE[0]%2&3$'
      HDR250(61) = 'DETECT LOCAL DIURNAL SUN SET -SUNSET[0]%2&3$'
      HDR250(62) = 'DETECT LOCAL DIURNAL MAXIMUM SOLAR ELEVATION'//
     &     ' -SUNMAXEL[0]%2&3$'
      HDR250(63) = 'DETECT LOCAL DIURNAL MINIMUM SOLAR ELEVATION'//
     &     ' -SUNMINEL[0]%2&3$'
      HDR250(64) = 'DETECT LOCAL DIURNAL CIVIL TWILIGHT START'//
     &     ' -CIVSTART[0]%2&3$'
      HDR250(65) = 'DETECT LOCAL DIURNAL CIVIL TWILIGHT STOP'//
     &     ' -CIVSTOP[0]%2&3$'
      HDR250(66) = 'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT START'//
     &     ' -NAUSTART[0]%2&3$'
      HDR250(67) = 'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT STOP'//
     &     ' -NAUSTOP[0]%2&3$'
      HDR250(68) = 'DETECT LOCAL DIURNAL ASTRONOMICAL '//
     &     'TWILIGHT START -ASTSTART[0]%2&3$'
      HDR250(69) = 'DETECT LOCAL DIURNAL ASTRONOMICAL '//
     &     'TWILIGHT STOP -ASTSTOP[0]%2&3$'
      HDR250(70) = 'DETECT LOCAL DIURNAL NIGHT START'//
     &     ' -NIGHTSTART[0]%2&3$'
      HDR250(71) = 'DETECT LOCAL DIURNAL NIGHT STOP'//
     &     ' -NIGHTSTOP[0]%2&3$'
      HDR250(75) = 'DETECT LOCAL DIURNAL SUN AZIMUTH '//
     &     '(0=NORTH, 90=EAST) -SUNAZI[*]%2&3$'
      HDR250(76) = 'DETECT LOCAL DIURNAL APPARENT SOLAR TIME '//
     &     '-SOLARTIME[*]%2&3$'
      HDR250(77) = 'DETECT LOCAL DIURNAL APPARENT LUNAR TIME '//
     &     '-LUNARTIME[*]%2&3$'
      HDR250(80) = 'DETECT LOCAL DIURNAL MOON RISE -MOONRISE[0]%2&3$'
      HDR250(81) = 'DETECT LOCAL DIURNAL MOON SET -MOONSET[0]%2&3$'
      HDR250(82) = 'DETECT LOCAL DIURNAL MAXIMUM MOON ELEVATION'//
     &     '  -MOONMAXEL[0]%2&3$'
      HDR250(83) = 'DETECT LOCAL DIURNAL MINIMUM MOON ELEVATION'//
     &     '  -MOONMINEL[0]%2&3$'
      HDR250(84) = 'DETECT LOCAL DIURNAL MOON AZIMUTH '//
     &     '(0=NORTH, 90=EAST) -MOONAZI[*]%2&3$'

      HDR250(90) = 'DETECT LOCAL POLAR SUN DAY START'//
     &     ' -POLSUNDAYSTART[0]%2&3$'
      HDR250(91) = 'DETECT LOCAL POLAR SUN DAY STOP'//
     &     ' -POLSUNDAYSTOP[0]%2&3$'
      HDR250(92) = 'DETECT LOCAL POLAR SUN NIGHT START'//
     &     ' -POLSUNNIGHTSTART[0]%2&3$'
      HDR250(93) = 'DETECT LOCAL POLAR SUN NIGHT STOP'//
     &     ' -POLSUNNIGHTSTOP[0]%2&3$'
      HDR250(94) = 'DETECT LOCAL POLAR LUNAR DAY START'//
     &     ' -POLMOONDAYSTART[0]%2&3$'
      HDR250(95) = 'DETECT LOCAL POLAR LUNAR DAY STOP'//
     &     ' -POLMOONDAYSTOP[0]%2&3$'
      HDR250(96) = 'DETECT LOCAL POLAR LUNAR NIGHT START'//
     &     ' -POLMOONNIGHTSTART[0]%2&3$'
      HDR250(97) = 'DETECT LOCAL POLAR LUNAR NIGHT STOP'//
     &     ' -POLMOONNIGHTSTOP[0]%2&3$'

      HDR250(98) = 'DETECT LOCAL SOLAR ECLIPSE '//
     &     '(MINOCC MAXOCC) -SOLECLMM[*]%2&3$'
      HDR250(99) = 'DETECT LOCAL SOLAR ECLIPSE '//
     &     '-SOLECL[0]%2&3$'
      HDR250(100) = 'STATE DATA AT LOCATIONS FOR DATES '//
     &     '-STATE[*]%2&3$'
C     
      WRITE(*,*) MYNAME,'----------------------------------------'
C     
C     READ DATA FROM INPUT FILE..............................
C     
      nArg=iArgC() ! number of command line arguments
      if (nArg .ne. 0) then
         lenbb=0
         call chop0(buff1000,1000)
         do ii=1,nArg
            call getArg(ii,buff250)
            call chop0(buff250,250)
            lenb=length(buff250,250,1)
            buff1000=buff1000(1:lenbb)//" "//buff250(1:lenb)
            lenbb=lenbb+1+lenb
            if (lenbb.gt.1000) then
               write(*,*)myname,'Too long command line argument.',
     &              buff1000(1:lenbb)
               irc=745
               return
            end if
         end do
         call chop0(buff1000,lenbb)
         lenbb=length(buff1000,1000,lenbb)
      end if

      KODE=-1
      if (nArg .ne. 0) then
         CALL NUKES(KODE,BUFF1000,HDR250,INTOUT,DAT250,ENDOFF,IRC)
      else
         CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
      end if
      IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'Error return from NUKE.',IRC
         RETURN
      END IF
      LINE=INTOUT
C     
      KODE=0
      DO WHILE (.NOT.ENDOFF)
C     
C     READ NEXT DATA LINE
C     
         if (nArg .ne. 0) then
            CALL NUKES(KODE,BUFF1000,HDR250,INTOUT,DAT250,ENDOFF,IRC)
         else
            CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
         end if
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from NUKE.',IRC
            RETURN
         END IF
         LINE=INTOUT
C     
         if (bdeb)WRITE(*,*) MYNAME,'Debug: Read header:',LINE
C     
C     CHECK WHAT LINE WE JUST READ
C     
         IF (LINE.EQ.1) THEN
         ELSEIF (LINE.EQ.2) THEN ! JPL file
            jpl1024 = DAT250
            LFLDAT(LINE) = .TRUE.
         ELSEIF (LINE.EQ.3) THEN ! start date
C     READ START DATE
            LEND=LENGTH(DAT250,250,22)
            CALL NUKECP(DAT250,'/',' ',LEND)
            CALL NUKECP(DAT250,':',' ',LEND)
            CALL NUKECP(DAT250,'-',' ',LEND)
            LEND=LENGTH(DAT250,250,1)
            SEC1=0.0
            READ(DAT250(1:LEND),*,END=201,ERR=201)
     &           YY1,MM1,DD1,HH1,MI1,sec1
            CALL DATE2JD(TSTARTJD,YY1,MM1,DD1,HH1,MI1,SEC1)
            LFLDAT(LINE)=.TRUE.
         ELSEIF (LINE.EQ.4) THEN ! end date
C     READ END DATE
            LEND=LENGTH(DAT250,250,22)
            CALL NUKECP(DAT250,'/',' ',LEND)
            CALL NUKECP(DAT250,':',' ',LEND)
            CALL NUKECP(DAT250,'-',' ',LEND)
            LEND=LENGTH(DAT250,250,1)
            SEC1=0.0D0
            READ(DAT250(1:LEND),*,END=201,ERR=201)
     &           YY1,MM1,DD1,HH1,MI1
            CALL DATE2JD(TENDJD,YY1,MM1,DD1,HH1,MI1,SEC1)
            LFLDAT(LINE)=.TRUE.
         ELSEIF (LINE.EQ.5) THEN ! report all events
            lall=.true.
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.6) THEN ! report next event
            lnext=.true.
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.7) THEN ! report prev event
            lprev=.true.
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.8) THEN ! number of decimals in seconds
            LEND=LENGTH(DAT250,250,1)
            READ(DAT250(1:LEND),*,END=201,ERR=201) secdec
            secdec=min(9,max(0,secdec))
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.9) THEN ! locations
            NLOC=NLOC+1
            IF (NLOC.GT.MAXLOC) THEN
               WRITE(*,*) MYNAME,'Too many locations.',nloc,maxloc
               IRC=232
               RETURN
            END IF
            DATX250=DAT250
            buff250 = NUKEHEAD(DATX250,250)
            lenb=length(buff250,250,10)
            READ(buff250(1:LENB),*,END=201,ERR=201)
     &           loclat(nloc)
            buff250 = NUKEHEAD(DATX250,250)
            lenb=length(buff250,250,10)
            READ(buff250(1:LENB),*,END=201,ERR=201)
     &           loclon(nloc)
            buff250 = NUKEHEAD(DATX250,250)
            lenb=length(buff250,250,10)
            READ(buff250(1:LENB),*,END=201,ERR=201)
     &           lochgt(nloc)
            lend=length(datx250,250,10)
            locid100(nloc)=datx250(1:min(100,lend))
            LFLDAT(LINE) = .TRUE.
         ELSEIF (LINE.EQ.10) THEN !  'REPORT INITIAL MOON STATE'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.11) THEN !  'REPORT MOON POSITION AT TIME INCREMENT'
            lend=length(dat250,250,10)
            READ(DAT250(1:LEND),*,END=201,ERR=201)
     &           DTMOON
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.12) THEN !  'REPORT INITIAL SUN STATE'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.13) THEN !  'REPORT SUN POSITION AT TIME INCREMENT'
            lend=length(dat250,250,10)
            READ(DAT250(1:LEND),*,END=201,ERR=201)
     &           DTSUN
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.14) THEN !  'REPORT POSITIONS AT TIME INCREMENT'
            lend=length(dat250,250,10)
            READ(DAT250(1:LEND),*,END=201,ERR=201)
     &           DTSS
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.15) THEN !  'DETECT WINTER SOLSTICE -WINTER[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.16) THEN !  'DETECT VERNAL EQUINOX -VERNAL[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.17) THEN !  'DETECT SUMMER SOLSTICE -SUMMER[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.18) THEN !  'DETECT AUTUMNAL EQUINOX -AUTUMNAL[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.19) THEN !  'DETECT EARTH IN PERIHELION -PERIHELION[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.20) THEN !  'DETECT EARTH IN APHELION -APHELION[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.21) THEN !  'DETECT NEW MOON (PHASE=0/100) -MOONNEW[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.22) THEN !  'DETECT FIRST QUARTER MOON (PHASE=25) -MOONFIRST[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.23) THEN !  'DETECT FULL MOON (PHASE=50) -MOONFULL[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.24) THEN !  'DETECT LAST QUARTER MOON (PHASE=75) -MOONLAST[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.25) THEN !  'DETECT MOON PHASE (0 TO 100) -MOONPHASE[*]'
            nphase=nphase+1
            if (nphase.gt.maxphase) then
               write(*,*)myname,'Too many phases.',maxphase,nphase
               irc=234
               return
            end if
            LEND=LENGTH(DAT250,250,1)
            read(dat250(1:lend),*,END=201,ERR=201) phase(nphase)
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.26) THEN !  'DETECT MOON ILLUMINATION MINIMUM -MINMOONILLUM[*]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.27) THEN !  'DETECT MOON ILLUMINATION MAXIMUM -MAXMOONILLUM[*]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.28) THEN !  'DETECT MOON ILLUMINATION (0 TO 100) -MOONILLUM[*]'
            nillum=nillum+1
            if (nillum.gt.maxillum) then
               write(*,*)myname,'Too many illums.',maxillum,nillum
               irc=234
               return
            end if
            LEND=LENGTH(DAT250,250,1)
            read(dat250(1:lend),*,END=201,ERR=201) illum(nillum)
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.30) THEN !  'DETECT MERCURY INFERIOR CONJUNCTION -MERINFCON[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.31) THEN !  'DETECT MERCURY SUPERIOR CONJUNCTION -MERSUPCON[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.32) THEN !  'DETECT MERCURY GREATEST WESTERN ELONGATION -MERWESTEL[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.33) THEN !  'DETECT MERCURY GREATEST EASTERN ELONGATION -MEREASTEL[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.34) THEN !  'DETECT VENUS INFERIOR CONJUNCTION -VENINFCON[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.35) THEN !  'DETECT VENUS GREATEST WESTERN ELONGATION -VENWESTEL[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.36) THEN !  'DETECT VENUS SUPERIOR CONJUNCTION -VENSUPCON[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.37) THEN !  'DETECT VENUS GREATEST EASTERN ELONGATION -VENEASTEL[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.38) THEN !  'DETECT MARS CONJUNCTION -MARCON[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.39) THEN !  'DETECT MARS WESTERN QUADRATURE -MARWEST[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.40) THEN !  'DETECT MARS OPPOSITION -MAROPP[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.41) THEN !  'DETECT MARS EASTERN QUADRATURE -MAREAST[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.42) THEN !  'DETECT JUPITER CONJUNCTION -JUPCON[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.43) THEN !  'DETECT JUPITER WESTERN QUADRATURE -JUPWEST[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.44) THEN !  'DETECT JUPITER OPPOSITION -JUPOPP[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.45) THEN !  'DETECT JUPITER EASTERN QUADRATURE -JUPEAST[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.46) THEN !  'DETECT SATURN CONJUNCTION -SATCON[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.47) THEN !  'DETECT SATURN WESTERN QUADRATURE -SATWEST[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.48) THEN !  'DETECT SATURN OPPOSITION -SATOPP[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.49) THEN !  'DETECT SATURN EASTERN QUADRATURE -SATEAST[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.50) THEN !  'DETECT MERCURY TRANSIT (ANYWHERE ON EARTH) -MERTRA[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.52) THEN !  'DETECT VENUS TRANSIT (ANYWHERE ON EARTH) -VENTRA[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.55) THEN !  'DETECT LUNAR ECLIPSE (MIN OCC, MAX OCC)) -LUNECLMMT[*]
            nmocc=nmocc+1
            if (nmocc.gt.maxmocc) then
               write(*,*)myname,'Too many Lunar Eclipse requests',
     &              nmocc,maxmocc
               irc=145
               return
            end if
            DATX250=DAT250
            buff250 = NUKEHEAD(DATX250,250)
            lenb=length(buff250,250,10)
            READ(buff250(1:LENB),*,iostat=irc2)
     &           moccmin(nmocc)
            lmoccmin(nmocc)=(irc2.eq.0)
            buff250 = NUKEHEAD(DATX250,250)
            lenb=length(buff250,250,10)
            READ(buff250(1:LENB),*,iostat=irc2)
     &         moccmax(nmocc)  
            lmoccmax(nmocc)=(irc2.eq.0)
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.56) THEN !  'DETECT LUNAR ECLIPSE -LUNECL[0]'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.57) THEN !  'REPORT VISIBLE MOON'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.58) THEN !  'REPORT VISIBLE SUN'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.60) THEN !  'DETECT LOCAL DIURNAL SUN RISE -SUNRISE[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.61) THEN !  'DETECT LOCAL DIURNAL SUN SET -SUNSET[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.62) THEN !  'DETECT LOCAL DIURNAL MAXIMUM SOLAR ELEVATION -SUNMAXEL[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.63) THEN !  'DETECT LOCAL DIURNAL MINIMUM SOLAR ELEVATION -SUNMINEL[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.64) THEN !  'DETECT LOCAL DIURNAL CIVIL TWILIGHT START -CIVSTART[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.65) THEN !  'DETECT LOCAL DIURNAL CIVIL TWILIGHT STOP -CIVSTOP[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.66) THEN !  'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT START -NAUSTART[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.67) THEN !  'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT STOP -NAUSTOP[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.68) THEN !  'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT START -ASTSTART[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.69) THEN !  'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT STOP -ASTSTOP[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.70) THEN !  'DETECT LOCAL DIURNAL NIGHT START -NIGHTSTART[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.71) THEN !  'DETECT LOCAL DIURNAL NIGHT STOP -NIGHTSTOP[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.75) THEN !  'DETECT LOCAL DIURNAL SUN AZIMUTH (0=NORTH, 90=EAST) -SUNAZI[*]%2&3&$'
            nsunazi=nsunazi+1
            if (nsunazi.gt.maxsunazi) then
               write(*,*)myname,'Too many sun azimuth requests',
     &              nsunazi,maxsunazi
               irc=145
               return
            end if
            LEND=LENGTH(DAT250,250,1)
            read(dat250(1:lend),*,END=201,ERR=201) sunazi(nsunazi)
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.76) THEN !  'DETECT LOCAL DIURNAL APPARENT SOLAR TIME -SOLARTIME[*]%2&3&$'
            nsolartime=nsolartime+1
            if (nsolartime.gt.maxsolartime) then
               write(*,*)myname,'Too many solar time requests',
     &              nsolartime,maxsolartime
               irc=145
               return
            end if
            LEND=LENGTH(DAT250,250,1)
            read(dat250(1:lend),*,END=201,ERR=201) solartime(nsolartime)
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.77) THEN !  'DETECT LOCAL DIURNAL APPARENT LUNAR TIME -LUNARTIME[*]%2&3&$'
            nlunartime=nlunartime+1
            if (nlunartime.gt.maxlunartime) then
               write(*,*)myname,'Too many lunar time requests',
     &              nlunartime,maxlunartime
               irc=145
               return
            end if
            LEND=LENGTH(DAT250,250,1)
            read(dat250(1:lend),*,END=201,ERR=201) lunartime(nlunartime)
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.80) THEN !  'DETECT LOCAL DIURNAL MOON RISE -MOONRISE[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.81) THEN !  'DETECT LOCAL DIURNAL MOON SET -MOONSET[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.82) THEN !  'DETECT LOCAL DIURNAL MAXIMUM MOON ELEVATION  -MOONMAXEL[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.83) THEN !  'DETECT LOCAL DIURNAL MINIMUM MOON ELEVATION  -MOONMINEL[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.84) THEN !  'DETECT LOCAL DIURNAL MOON AZIMUTH (0=NORTH, 90=EAST) -MOONAZI[*]%2&3&$'
            nmoonazi=nmoonazi+1
            if (nmoonazi.gt.maxmoonazi) then
               write(*,*)myname,'Too many moon azimuth requests',
     &              nmoonazi,maxmoonazi
               irc=146
               return
            end if
            LEND=LENGTH(DAT250,250,1)
            read(dat250(1:lend),*,END=201,ERR=201) moonazi(nmoonazi)
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.90) THEN !  'DETECT LOCAL POLAR DAY START -POLDAYSTART[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.91) THEN !  'DETECT LOCAL POLAR DAY STOP -POLDAYSTOP[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.92) THEN !  'DETECT LOCAL POLAR NIGHT START -POLNIGHTSTART[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.93) THEN !  'DETECT LOCAL POLAR NIGHT STOP -POLNIGHTSTOP[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.94) THEN !  'DETECT LOCAL POLAR DAY START -POLDAYSTART[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.95) THEN !  'DETECT LOCAL POLAR DAY STOP -POLDAYSTOP[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.96) THEN !  'DETECT LOCAL POLAR NIGHT START -POLNIGHTSTART[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.97) THEN !  'DETECT LOCAL POLAR NIGHT STOP -POLNIGHTSTOP[0]%2&3&$'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.98) THEN !  'DETECT LOCAL SOLAR ECLIPSE (MINOCC MAXOCC) -SOLECLMM'
            nsocc=nsocc+1
            if (nsocc.gt.maxsocc) then
               write(*,*)myname,'Too many Lunar Eclipse requests',
     &              nsocc,maxsocc
               irc=145
               return
            end if
            DATX250=DAT250
            buff250 = NUKEHEAD(DATX250,250)
            lenb=length(buff250,250,10)
            READ(buff250(1:LENB),*,iostat=irc2)
     &           soccmin(nsocc)
            lsoccmin(nsocc)=(irc2.eq.0)
            buff250 = NUKEHEAD(DATX250,250)
            lenb=length(buff250,250,10)
            READ(buff250(1:LENB),*,iostat=irc2)
     &         soccmax(nsocc)  
            lsoccmax(nsocc)=(irc2.eq.0)
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.99) THEN !  'DETECT LOCAL SOLAR ECLIPSE -SOLECL'
            LFLDAT(LINE) =.TRUE.
         ELSEIF (LINE.EQ.100) THEN !  'STATE DATES'
            ndtg=min(ndtg+1,mdtg)
            dtg30(ndtg)=dat250(1:30)
            LFLDAT(LINE) =.TRUE.
         ELSE IF (LINE.NE.0) THEN ! LINE.EQ.0 IMPLIES SOMETIMES EOF
            WRITE(*,*) MYNAME,
     &           'System error, line not implemented:',LINE
            IRC=999
            RETURN
         ENDIF
      ENDDO
C     
      KODE=1
      if (nArg .ne. 0) then
         CALL NUKES(KODE,BUFF1000,HDR250,INTOUT,DAT250,ENDOFF,IRC)
      else
         CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
      end if
      IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'Error return from NUKE.',IRC
         RETURN
      END IF
C     
      GOTO 202
 201  CONTINUE
      LEND=LENGTH(DAT250,250,1)
      CALL CHOP0(HDR250(LINE),250)
      LENH=LENGTH(HDR250(LINE),250,1)
      WRITE(*,*) MYNAME,'unable to read: '//
     &     DAT250(1:LEND)//
     &     ' (HDR='//HDR250(LINE)(1:LENH)//')'
      IRC=522
      RETURN
 202  CONTINUE
C     
      WRITE(*,*) MYNAME,'----------------------------------------'
C
C     initialise common-blocks
C
      call JPLephOpen(jpl1024,crc250,irc) ! open JPL ephemeride file for reading
      if (irc.ne.0) then
         write(buff250,*)' < '//myname,
     &        ' > Error return from JPLEPHOPEN.',
     &        irc,eventId
         lencrc=length(crc250,250,10)
         crc250=crc250(1:lencrc)//buff250
         call chop0(crc250,250)
         return
      end if
C
      nrep(:)=0
      if (lprev.and.lnext) then ! search for previous and next event
         searchCode=0
      else if (lprev) then      ! search for previous event only
         searchCode=-1
      else if (lnext) then      ! search for next event only
         searchCode=+1
      else                      ! report for all events until tendJD
         searchCode=+2          ! should only arrive here if lall=.true.
      end if
C
C     Almanakk for norge uses:
C     682465.486092342  ! Sun
C     1738.327257429 ! Moon
C     6043.753975916 ! Venus
C     3395.871664686 ! Mars
C     71403.063680814 ! Jupiter
C     60003.70593416 ! Saturn
C
C     Planet  mass (kg) radius (m)
C     Mercury 3.3023  2,440,000
C     Venus   4.87D24 6,051,000
C     Earth   5.97D24 6,378,000
C     Moon    7.35D22 1,738,000
C     Mars    6.42D23 3,397,000
C     Jupiter 1.90D27 71,492,000
C     Saturn  5.69D26 60,268,000
C     Uranus  8.66D25 25,559,000
C     Neptune 1.03D26 24,764,000
C     Pluto   1.31D22 1,160,000
C     
C
C     Define objects that we want the position for
C
C      rSun     = 682500.0D0
C      rMercury =   2440.0D0
C      rVenus   =   6051.0D0
C      rMars    =   3397.0D0
C      rJupiter =  71492.0D0
C      rSaturn  =  60268.0D0
C      rUranus  =  25559.0D0
C      rNeptune =  24764.0D0
C      rPluto   =   1160.0D0
C      rMoon    =   1738.0D0
C      rEarth   =   6378.0D0
C
      IF (LFLDAT(10)) THEN      !  'REPORT INITIAL MOON STATE'
         eventId=100
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,    ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)' < '//myname,
     &              ' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (LFLDAT(11)) THEN      !  'REPORT MOON POSITION AT TIME INCREMENT'
         eventId=110
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            eventVal(4)=dtmoon
            neventVal=4         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,    ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)' < '//myname,
     &              ' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (LFLDAT(12)) THEN      !  'REPORT INITIAL SUN STATE'
         eventId=120
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)' < '//myname,
     &              ' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc+1)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (LFLDAT(13)) THEN      !  'REPORT SUN POSITION AT TIME INCREMENT'
         eventId=130
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            eventVal(4)=dtsun
            neventVal=4         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)' < '//myname,
     &              ' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (LFLDAT(14)) THEN      !  'REPORT POSITIONS AT TIME INCREMENT'
         eventId=1000
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            eventVal(4)=dtss
            neventVal=4         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)' < '//myname,
     &              ' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (LFLDAT(15)) THEN      !  'DETECT WINTER SOLSTICE -WINTER[0]'
         eventId=150
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)' < '//myname,
     &           ' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      
      IF (lfldat(16)) THEN  !  'DETECT VERNAL EQUINOX -VERNAL[0]'
         eventId=160
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF

      IF (lfldat(17)) THEN      !  'DETECT SUMMER SOLSTICE -SUMMER[0]'
         eventId=170
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(18)) THEN      !  'DETECT AUTUMNAL EQUINOX -AUTUMNAL[0]'
         eventId=180
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF

      IF (lfldat(19)) THEN  !  'DETECT EARTH IN PERIHELION -PERIHELION[0]'
         eventId=190
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(20)) THEN      !  'DETECT EARTH IN APHELION -APHELION[0]'
         eventId=200
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF

      IF (lfldat(21)) THEN      !  'DETECT NEW MOON (PHASE=0/100) -MOONNEW[0]'
         eventId=210
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF

      IF (lfldat(22)) THEN      !  'DETECT FIRST QUARTER MOON (PHASE=25) -MOONFIRST[0]'
         eventId=220
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(23)) THEN      !  'DETECT FULL MOON (PHASE=50) -MOONFULL[0]'
         eventId=230
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(24)) THEN      !  'DETECT LAST QUARTER MOON (PHASE=75) -MOONLAST[0]'
         eventId=240
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(25)) THEN      !  'DETECT MOON PHASE (0 TO 100) -MOONPHASE[*]'
         eventId=250
         do jj=1,nphase
            eventVal(1)=phase(jj) ! target phase
            ii=0
            neventVal=1         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(26)) THEN      !  'DETECT MOON ILLUMINATION MINIMUM -MINMOONILLUM[*]'
         eventId=260
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(27)) THEN      !  'DETECT MOON ILLUMINATION MAXIMUM -MAXMOONILLUM[*]'
         eventId=270
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(28)) THEN      !  'DETECT MOON ILLUMINATION (0 TO 100) -MOONILLUM[*]'
         eventId=280
         do jj=1,nillum
            eventVal(1)=illum(jj) ! target illumination
            ii=0
            neventVal=1         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
C
C Mercury 4° 6' 0"      4.1 deg/day           87.97 Earth Days   0.24 Earth Years 
C Venus   1° 36' 0"     1.6 deg/day          224.70 Earth days 
C The Earth 0° 59' 8"   0.98556 deg/day      365.25 days
C Mars    0° 31' 0"     0.527 deg/day        686.93 Earth days 365.26 Earth Days 
C Jupiter 0° 4'59"      0.0830556 deg/day   4331.89 days   11.86 Earth years 
C Saturn  0° 2' 1"      0.033611 deg/day   10760.34 days   29.46 Earth years 
C Uranus  0° 0' 42"     0.0116667 deg/day  30589.90 days   83.75 Earth years 
C Neptune 0° 0' 24"     0.0066667 deg/day  59799.14 days  163.72 Earth years 
C Pluto   0° 0' 14"     0.0038889 deg/day  90553.40 days  247.92 Earth years 
C The Moon (around the Earth) 13° 10'35"  13.176389 deg/day 27.3216 days
C
      IF (lfldat(30)) THEN      ! 'DETECT MERCURY INFERIOR CONJUNCTION -MERINFCON[0]'
         eventId=300
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(31)) THEN      !  'DETECT MERCURY SUPERIOR CONJUNCTION -MERSUPCON[0]'
         eventId=310
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in jd2000)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(32)) THEN      !  'DETECT MERCURY GREATEST WESTERN ELONGATION -MERWESTEL[0]'
         eventId=320
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(33)) THEN      !  'DETECT MERCURY GREATEST EASTERN ELONGATION -MEREASTEL[0]'
         eventId=330
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(34)) THEN      !  'DETECT VENUS INFERIOR CONJUNCTION -VENINFCON[0]'
         eventId=340
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(35)) THEN      !  'DETECT VENUS GREATEST WESTERN ELONGATION -VENWESTEL[0]'
         eventId=350
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(36)) THEN      !  'DETECT VENUS SUPERIOR CONJUNCTION -VENSUPCON[0]'
         eventId=360
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in jd2000)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(37)) THEN      !  'DETECT VENUS GREATEST EASTERN ELONGATION -VENEASTEL[0]'
         eventId=370
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(38)) THEN      !  'DETECT MARS CONJUNCTION -MARCON[0]'
         eventId=380
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(39)) THEN      !  'DETECT MARS WESTERN QUADRATURE -MARWEST[0]'
         eventId=390
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(40)) THEN      !  'DETECT MARS OPPOSITION -MAROPP[0]'
         eventId=400
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(41)) THEN      !  'DETECT MARS EASTERN QUADRATURE -MAREAST[0]'
         eventId=410
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(42)) THEN      !  'DETECT JUPITER CONJUNCTION -JUPCON[0]'
         eventId=420
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(43)) THEN      !  'DETECT JUPITER WESTERN QUADRATURE -JUPWEST[0]'
         eventId=430
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(44)) THEN      !  'DETECT JUPITER OPPOSITION -JUPOPP[0]'
         eventId=440
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(45)) THEN      !  'DETECT JUPITER EASTERN QUADRATURE -JUPEAST[0]'
         eventId=450
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(46)) THEN      !  'DETECT SATURN CONJUNCTION -SATCON[0]'
         eventId=460
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(47)) THEN      !  'DETECT SATURN WESTERN QUADRATURE -SATWEST[0]'
         eventId=470
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(48)) THEN      !  'DETECT SATURN OPPOSITION -SATOPP[0]'
         eventId=480
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(49)) THEN      !  'DETECT SATURN EASTERN QUADRATURE -SATEAST[0]'
         eventId=490
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(50)) THEN      !  'DETECT MERCURY TRANSIT (ANYWHERE ON EARTH) -MERTRA[0]'
         eventId=500
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(52)) THEN      !  'DETECT VENUS TRANSIT (ANYWHERE ON EARTH) -VENTRA[0]'
         eventId=520
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in jd2000)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(55)) THEN      !  'DETECT LUNAR ECLIPSE (MIN OCC...
         eventId=550
         ii=0
         do jj=1,nmocc
            eventVal(1)=moccmin(jj)
            eventVal(2)=moccmax(jj)
            neventVal=2         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(56)) THEN      !  'DETECT MOON IN UMBRA -UMBRA[0]'
         eventId=560
         ii=0
         neventVal=0            ! number of event values
         call astroEvent(
     &        tstartJD,       ! start time (in julianDay)
     &        searchCode,        ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &        tendJD,         ! report all events end time (in julianDay)
     &        eventId,          ! requested event id (SEE TABLE BELOW)
     &        neventVal,        ! number of event input data (SEE TABLE BELOW)
     &        eventVal,         ! event input data (SEE TABLE BELOW)
     &        maxrep,           ! maximum number of output reports
     &        nrep(ii),         ! number of output reports
     &        repJD(1,ii),    ! time of output report (in julianDay)
     &        repId(1,ii),      ! output report identification (SEE TABLE BELOW)
     &        repVal(1,ii),     ! output report value (SEE TABLE BELOW)
     &        rep250(1,ii),     ! output report string (redundant description)
     &        secdec,           ! number of second decimals used in output report string 
     &        crc250,           ! error return string
     &        irc)              ! error return code (0=ok)
         if (irc.ne.0) then
            write(buff250,*)
     &           ' < '//myname,' > Error return from astroEvent.',
     &           irc,eventId
            lencrc=length(crc250,250,10)
            crc250=crc250(1:lencrc)//buff250
            call chop0(crc250,250)
            return
         end if
      END IF
      IF (lfldat(57)) THEN      !  'DETECT MOON IN UMBRA -UMBRA[0]'
         eventId=105
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,    ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(58)) THEN      !  'DETECT MOON IN UMBRA -UMBRA[0]'
         eventId=125
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,    ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(60)) THEN      !  'DETECT LOCAL DIURNAL SUN RISE -SUNRISE[0]%2&3&$'
         eventId=600
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(61)) THEN      !  'DETECT LOCAL DIURNAL SUN SET -SUNSET[0]%2&3&$'
         eventId=610
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      end if
      IF (lfldat(62)) THEN      !  'DETECT LOCAL DIURNAL MAXIMUM SOLAR ELEVATION -SUNMAXEL[0]%2&3&$'
         eventId=620
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(63)) THEN      !  'DETECT LOCAL DIURNAL MINIMUM SOLAR ELEVATION -SUNMINEL[0]%2&3&$'
         eventId=630
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(64)) THEN      !  'DETECT LOCAL DIURNAL CIVIL TWILIGHT START -CIVSTART[0]%2&3&$'
         eventId=640
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(65)) THEN      !  'DETECT LOCAL DIURNAL CIVIL TWILIGHT STOP -CIVSTOP[0]%2&3&$'
         eventId=650
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(66)) THEN      !  'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT START -NAUSTART[0]%2&3&$'
         eventId=660
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(67)) THEN      !  'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT STOP -NAUSTOP[0]%2&3&$'
         eventId=670
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(68)) THEN      !  'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT START -ASTSTART[0]%2&3&$'
         eventId=680
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(69)) THEN      !  'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT STOP -ASTSTOP[0]%2&3&$'
         eventId=690
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(70)) THEN      !  'DETECT LOCAL DIURNAL NIGHT START -NIGHTSTART[0]%2&3&$'
         eventId=700
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(71)) THEN      !  'DETECT LOCAL DIURNAL NIGHT STOP -NIGHTSTOP[0]%2&3&$'
         eventId=710
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(75)) THEN      !  'DETECT LOCAL DIURNAL SUN AZIMUTH (0=NORTH, 90=EAST) -SUNAZI[*]%2&3&$'
         eventId=750
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            do jj=1,nsunazi
               eventVal(4)=sunazi(jj)
               neventVal=4      ! number of event values
               call astroEvent(
     &              tstartJD, ! start time (in julianDay)
     &              searchCode,  ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &              tendJD,   ! report all events end time (in julianDay)
     &              eventId,    ! requested event id (SEE TABLE BELOW)
     &              neventVal,  ! number of event input data (SEE TABLE BELOW)
     &              eventVal,   ! event input data (SEE TABLE BELOW)
     &              maxrep,     ! maximum number of output reports
     &              nrep(ii),   ! number of output reports
     &              repJD(1,ii), ! time of output report (in julianDay)
     &              repId(1,ii), ! output report identification (SEE TABLE BELOW)
     &              repVal(1,ii), ! output report value (SEE TABLE BELOW)
     &              rep250(1,ii), ! output report string (redundant description)
     &              secdec,     ! number of second decimals used in output report string 
     &              crc250,     ! error return string
     &              irc)        ! error return code (0=ok)
               if (irc.ne.0) then
                  write(buff250,*)
     &                 ' < '//myname,' > Error return from astroEvent.',
     &                 irc,eventId
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//buff250
                  call chop0(crc250,250)
                  return
               end if
            end do
         end do
      END IF
      IF (lfldat(76)) THEN      !  'DETECT LOCAL DIURNAL APPARENT SOLAR TIME -SOLARTIME[*]%2&3&$'
         eventId=760
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            do jj=1,nsolartime
               eventVal(4)=solartime(jj)
               neventVal=4      ! number of event values
               call astroEvent(
     &              tstartJD, ! start time (in julianDay)
     &              searchCode,  ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &              tendJD,   ! report all events end time (in julianDay)
     &              eventId,    ! requested event id (SEE TABLE BELOW)
     &              neventVal,  ! number of event input data (SEE TABLE BELOW)
     &              eventVal,   ! event input data (SEE TABLE BELOW)
     &              maxrep,     ! maximum number of output reports
     &              nrep(ii),   ! number of output reports
     &              repJD(1,ii), ! time of output report (in julianDay)
     &              repId(1,ii), ! output report identification (SEE TABLE BELOW)
     &              repVal(1,ii), ! output report value (SEE TABLE BELOW)
     &              rep250(1,ii), ! output report string (redundant description)
     &              secdec,     ! number of second decimals used in output report string 
     &              crc250,     ! error return string
     &              irc)        ! error return code (0=ok)
               if (irc.ne.0) then
                  write(buff250,*)
     &                 ' < '//myname,' > Error return from astroEvent.',
     &                 irc,eventId
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//buff250
                  call chop0(crc250,250)
                  return
               end if
            end do
         end do
      END IF
      IF (lfldat(77)) THEN      !  'DETECT LOCAL DIURNAL APPARENT LUNAR TIME -LUNARTIME[*]%2&3&$'
         eventId=770
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            do jj=1,nlunartime
               eventVal(4)=lunartime(jj)
               neventVal=4      ! number of event values
               call astroEvent(
     &              tstartJD, ! start time (in julianDay)
     &              searchCode,  ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &              tendJD,   ! report all events end time (in julianDay)
     &              eventId,    ! requested event id (SEE TABLE BELOW)
     &              neventVal,  ! number of event input data (SEE TABLE BELOW)
     &              eventVal,   ! event input data (SEE TABLE BELOW)
     &              maxrep,     ! maximum number of output reports
     &              nrep(ii),   ! number of output reports
     &              repJD(1,ii), ! time of output report (in julianDay)
     &              repId(1,ii), ! output report identification (SEE TABLE BELOW)
     &              repVal(1,ii), ! output report value (SEE TABLE BELOW)
     &              rep250(1,ii), ! output report string (redundant description)
     &              secdec,     ! number of second decimals used in output report string 
     &              crc250,     ! error return string
     &              irc)        ! error return code (0=ok)
               if (irc.ne.0) then
                  write(buff250,*)
     &                 ' < '//myname,' > Error return from astroEvent.',
     &                 irc,eventId
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//buff250
                  call chop0(crc250,250)
                  return
               end if
            end do
         end do
      END IF
      IF (lfldat(80)) THEN      !  'DETECT LOCAL DIURNAL MOON RISE -MOONRISE[0]%2&3&$'
         eventId=800
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(81)) THEN      !  'DETECT LOCAL DIURNAL MOON SET -MOONSET[0]%2&3&$'
         eventId=810
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      end if
      IF (lfldat(82)) THEN      !  'DETECT LOCAL DIURNAL MAXIMUM MOON ELEVATION  -MOONMAXEL[0]%2&3&$'
         eventId=820
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(83)) THEN      !  'DETECT LOCAL DIURNAL MINIMUM MOON ELEVATION  -MOONMINEL[0]%2&3&$'
         eventId=830
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(84)) THEN      !  'DETECT LOCAL DIURNAL MOON AZIMUTH (0=NORTH, 90=EAST) -MOONAZI[*]%2&3&$'
         eventId=840
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            do jj=1,nmoonazi
               eventVal(4)=moonazi(jj)
               neventVal=4      ! number of event values
               call astroEvent(
     &              tstartJD, ! start time (in julianDay)
     &              searchCode,  ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &              tendJD,   ! report all events end time (in julianDay)
     &              eventId,    ! requested event id (SEE TABLE BELOW)
     &              neventVal,  ! number of event input data (SEE TABLE BELOW)
     &              eventVal,   ! event input data (SEE TABLE BELOW)
     &              maxrep,     ! maximum number of output reports
     &              nrep(ii),   ! number of output reports
     &              repJD(1,ii), ! time of output report (in julianDay)
     &              repId(1,ii), ! output report identification (SEE TABLE BELOW)
     &              repVal(1,ii), ! output report value (SEE TABLE BELOW)
     &              rep250(1,ii), ! output report string (redundant description)
     &              secdec,     ! number of second decimals used in output report string 
     &              crc250,     ! error return string
     &              irc)        ! error return code (0=ok)
               if (irc.ne.0) then
                  write(buff250,*)
     &                 ' < '//myname,' > Error return from astroEvent.',
     &                 irc,eventId
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//buff250
                  call chop0(crc250,250)
                  return
               end if
            end do
         end do
      END IF
      IF (lfldat(90)) THEN      !  'DETECT LOCAL POLAR SUN DAY START -POLDAYSTART[0]%2&3&$'
         eventId=900
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(91)) THEN      !  'DETECT LOCAL POLAR SUN DAY STOP -POLDAYSTOP[0]%2&3&$'
         eventId=910
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(92)) THEN      !  'DETECT LOCAL POLAR SUN NIGHT START -POLNIGHTSTART[0]%2&3&$'
         eventId=920
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(93)) THEN      !  'DETECT LOCAL POLAR SUN NIGHT STOP -POLNIGHTSTOP[0]%2&3&$'
         eventId=930
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(94)) THEN      !  'DETECT LOCAL POLAR LUNAR DAY START -POLDAYSTART[0]%2&3&$'
         eventId=940
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(95)) THEN      !  'DETECT LOCAL POLAR LUNAR DAY STOP -POLDAYSTOP[0]%2&3&$'
         eventId=950
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(96)) THEN      !  'DETECT LOCAL POLAR LUNAR NIGHT START -POLNIGHTSTART[0]%2&3&$'
         eventId=960
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(97)) THEN      !  'DETECT LOCAL POLAR LUNAR NIGHT STOP -POLNIGHTSTOP[0]%2&3&$'
         eventId=970
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
      IF (lfldat(98)) THEN      !  'DETECT LOCAL PARTIAL SOLAR ECLIPSE BY MOON START -ECLIPSEPARTSTART[0]%2&3&$'
         eventId=980
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            do jj=1,nsocc
               eventVal(4)=soccmin(jj)
               eventVal(5)=soccmax(jj)
               neventVal=5      ! number of event values
               call astroEvent(
     &              tstartJD, ! start time (in julianDay)
     &              searchCode,  ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &              tendJD,   ! report all events end time (in julianDay)
     &              eventId,    ! requested event id (SEE TABLE BELOW)
     &              neventVal,  ! number of event input data (SEE TABLE BELOW)
     &              eventVal,   ! event input data (SEE TABLE BELOW)
     &              maxrep,     ! maximum number of output reports
     &              nrep(ii),   ! number of output reports
     &              repJD(1,ii), ! time of output report (in julianDay)
     &              repId(1,ii), ! output report identification (SEE TABLE BELOW)
     &              repVal(1,ii), ! output report value (SEE TABLE BELOW)
     &              rep250(1,ii), ! output report string (redundant description)
     &              secdec,     ! number of second decimals used in output report string 
     &              crc250,     ! error return string
     &              irc)        ! error return code (0=ok)
               if (irc.ne.0) then
                  write(buff250,*)
     &                 ' < '//myname,' > Error return from astroEvent.',
     &                 irc,eventId
                  lencrc=length(crc250,250,10)
                  crc250=crc250(1:lencrc)//buff250
                  call chop0(crc250,250)
                  return
               end if
            end do
         end do
      END IF
      IF (lfldat(99)) THEN      !  'DETECT LOCAL PARTIAL SOLAR ECLIPSE BY MOON STOP -ECLIPSEPARTSTOP[0]%2&3&$'
         eventId=990
         do ii=1,nloc
            eventVal(1)=loclat(ii)
            eventVal(2)=loclon(ii)
            eventVal(3)=lochgt(ii)
            neventVal=3         ! number of event values
            call astroEvent(
     &           tstartJD,    ! start time (in julianDay)
     &           searchCode,     ! search Code (-1:prev,+1:next,0:both,2:report all events in period)
     &           tendJD,      ! report all events end time (in julianDay)
     &           eventId,       ! requested event id (SEE TABLE BELOW)
     &           neventVal,     ! number of event input data (SEE TABLE BELOW)
     &           eventVal,      ! event input data (SEE TABLE BELOW)
     &           maxrep,        ! maximum number of output reports
     &           nrep(ii),      ! number of output reports
     &           repJD(1,ii), ! time of output report (in julianDay)
     &           repId(1,ii),   ! output report identification (SEE TABLE BELOW)
     &           repVal(1,ii),  ! output report value (SEE TABLE BELOW)
     &           rep250(1,ii),  ! output report string (redundant description)
     &           secdec,        ! number of second decimals used in output report string 
     &           crc250,        ! error return string
     &           irc)           ! error return code (0=ok)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from astroEvent.',
     &              irc,eventId
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      END IF
C
      if (lfldat(100)) then ! state
         do jj=1, nloc
            call astrostate(ndtg,dtg30,loclat(jj),loclon(jj),
     &           1.0D0,crc250,irc)
            if (irc.ne.0) then
               write(buff250,*)
     &              ' < '//myname,' > Error return from state.',
     &              irc,ndtg
               lencrc=length(crc250,250,10)
               crc250=crc250(1:lencrc)//buff250
               call chop0(crc250,250)
               return
            end if
         end do
      end if
C
      call JPLephClose(jpl1024,crc250,irc)
C
C     PRINT REPORTS CHRONOLOGICALLY FOR EACH LOCATION...
C
      do ll=0,nloc
         call chop0(locid100(ll),100)
         lenl=length(locid100(ll),100,10)
C     sort reports chronologically
         do rr=1,nrep(ll)
            ind(rr)=rr
         end do
         call mergesort(nrep(ll),ind,repJD(1,ll))
         do rrx=1,nrep(ll)
            rr=ind(rrx)
            call chop0(rep250(rr,ll),250)
            lenr=length(rep250(rr,ll),250,20)
            write(*,*) repid(rr,ll),
     &           repval(rr,ll),locid100(ll)(1:lenl)//
     &           "  "//rep250(rr,ll)(1:lenr) ! ,repJD(rrx,ll)
         end do
      end do
C
      RETURN
      CONTAINS

    
      subroutine mergesort(n,ind,key)
      
      implicit none
      integer n
      integer, target :: ind(n)
      real, target :: key(n)
      
      integer, target, dimension(size(ind)) :: tmp
      
      call mergesort1(ind, key, tmp, n, 1, n)
      
      end subroutine mergesort

      recursive subroutine mergesort1(ind, key, tmp, n, l, r)
      
      implicit none
      
      integer n
      integer ind(n)
      real key(:)
      integer tmp(size(ind))
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
               ind(k) = tmp(j)
               j = j-1
            end if
         end do
      end if
      
      end subroutine mergesort1

      END

