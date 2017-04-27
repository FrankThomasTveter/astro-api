*******************************************************************************

 JPL PLANETARY AND LUNAR EPHEMERIDES : Export Information     [12 October 2007]

*******************************************************************************

INTRODUCTION
------------

  Selected JPL Planetary and Lunar Ephemerides are available to outside users.
  Subroutines are availabe to read the ephemerides to obtain the RECTANGULAR
  COORDINATES (x,y,z) of the sun, moon, and nine major planets at the desired
  general relativistic COORDINATE TIME (TDB). The user is then able to compute
  angular coordinate, light times, etc. as required. Such computations usually
  require significant programming experience and familiarity with Earth rotation
  calculations, access to variable Earth orientation calibrations, knowledge
  of relativistic effects, etc.

  We suggest for the more casual user, JPL's interactive website and telnet
  service, "Horizons", which provides a wide variety of astronomical 
  information and which uses the full precision of our ephemerides.  See 
  the website,  "http://ssd.jpl.nasa.gov/".

  For those who truly need the high-precision, basic JPL ephemerides, there 
  are two methods of acquiring the ephemerides: 1) from this site via ftp and 
  2) via a CRrom from the publisher, Willmann-Bell (see Appendix B).

  The final goal of the installation process is the successful execution 
  of the main program "TESTEPH.F".  This program reads and interpolates 
  planetary and lunar coordinates from a binary direct-read ephemeris file 
  and compares these results against corresponding numbers produced at JPL.  
  "TESTEPH.F" uses the subroutines which are of eventual interest to the user.

  It is strongly suggested that a potential user first read through this
  README in its entirety.  This will provide an idea of what is involved in
  both the installation and in the usage of the ephemeris package.

This README contains the following sections;

  Introduction
  Location of the files
  What to do (ftp and install)
  Unix users vs. other users
  Contents to be retrieved by the user (which files to get via ftp)
  Brief item description: what each retrieved item is for
  Available ephemerides (which ones; what years are covered)
  Ephemeris sizes (how big the files are)
  References (publications describing the ephemerides)
  Software description (the subroutines which provide ephemeris information)
  Software Usage (calling sequances, etc.)
  Constants on the Ephemeris Files : what ephemeris constants are on the files
  Assistance (only in case of emergency)
    Appendix A : The internal format of the ephemeris files
    Appendix B : CDrom available from Willmann-Bell
    Appendix C : Corrections to the software on the CDroms
    Appendix D : Documentation of DE405 and DE406
    Appendix E : Versions of the software in various languages (from
                 outside sources)

*******************************************************************************

LOCATION
--------

The ephemeris package is available over Internet from the anonymous ftp server:

           ssd.jpl.nasa.gov 

  When connected, "cd pub/eph/planets/"

******************************************************************************

WHAT TO DO
----------

  Retrieve the items, listed in the next sections, from the anonymous ftp site.

  Read the "usrguide", especially the parts about "NRECL" and "KSIZE"
   
  [Note: For UNIX-based users, some ephemeris files are available in binary 
  form, ready for immediate installation on the user's computer.  For all 
  others, the ephemeris is supplied in ASCII format and must be converted 
  (a one-time only process) into binary format for installation; software 
  is supplied for this conversion.

******************************************************************************

CONTENTS TO BE RETRIEVED BY THE USER
------------------------------------

 ***  UNIX users: retrieve the following 7 items: 

                 (via ftp from ssd.jpl.nasa.gov)

          /pub/eph/planets/usrguide
          /pub/eph/planets/fortran/testeph.f
          /pub/eph/planets/fortran/selcon.f
          /pub/eph/planets/fortran/binmerge.f
          /pub/eph/planets/fortran/binshort.f
          /pub/eph/planets/unix/deXXX/unxSYYYY.XXX  (multiple files)
          /pub/eph/planets/test-data/testpo.XXX

     where "XXX" is a three-digit number of the desired planetary ephemeris (e.g. 403),
     and "SYYYY" is the starting year of each particular ephemeris block 
     ("S" is the sign : "p" for + and "m" for -).

     The UNIX binary files come in 50-year blocks (3.8-4.7 Mbytes each, 
     depending upon the ephemeris).  Once they are installed on the user's 
     computer, contiguous blocks may be merged together to form a single 
     ephemeris file; this is done using "binmerge.f".

 ***  non-UNIX users: retrieve the following 9 items:

                 (one is in the directory :/usr1/ftp/" upon entry)

          /pub/eph/planets/usrguide
          /pub/eph/planets/fortran/testeph.f
          /pub/eph/planets/fortran/selcon.f
          /pub/eph/planets/fortran/asc2eph.f
          /pub/eph/planets/fortran/binmerge.f
          /pub/eph/planets/fortran/binshort.f
          /pub/eph/planets/ascii/DExxx/ascSYYYY.XXX  (multiple files)
          /pub/eph/planets/ascii/DExxx/header.XXX
          /pub/eph/planets/test-data/testpo.XXX

     where "SYYYY" is the starting year of each particular ephemeris block
     ("S" is the sign : "p" for + and "m" for -) and "XXX" is the ephemeris
     number: 200, 403, 405, or 406.  See choices below for different 
     ephemerides.

     The ASCII files come in 20-year blocks (about 3.5 Mbytes each). The 
     blocks are converted into binary blocks on the user's computer, using 
     "asc2eph.f"; then, contiguous binary blocks may be merged together to 
     form a single ephemeris file using "binmerge.f"; or, a smaller portion 
     of an ephemeris file may be extracted from a larger file using 
     "binshort.f".

******************************************************************************

BRIEF ITEM DESCRIPTION
-----------------------

  "usrguide"     : User's Guide containing instructions for obtaining and using
                   the ephemerides, including the usage of the subroutines and 
                   the testing of the software package.
                      
  "testeph"      : Software Package containing a main program which uses the 
                   reading and interpolating subroutines.  This program 
                   compares the results with similar runs made at JPL in order 
                   to ensure that the ephemeris is installed and being read 
                   correctly.
                   

                   The reading and interpolating subroutines included in 
                   "testeph", along with the ephemeris file(s) themselves, are 
                   the items which are of eventual interest to the user.
                   

  "testpo"       : Test results computed at JPL; these are input by the program
                   "testeph" and are used for testing the ephemeris 
                   installation.  There is a different "testpo" for each 
                   different ephemeris; they must match or the test will not 
                   work correctly.

  "binmerge"     : Program to merge two adjoining binary ephemerides.

  "binshort"     : Program to extract a portion of an ephemeris from a larger 
                   file.


  for UNIX users
  --------------

  "unxSYYYY.XXX" : Binary ephemeris files from JPL Ephemeris DEXXX, covering 50
                   years, starting in the year SYYYY ("p/m" for "+/-").  The 
                   pieces may be used separately, or the 50-year files 
                   (300-year files for DE406) may be merged into a single 
                   ephemeris file using the program, "binmerge.f".  Also, a 
                   shorter-span ephemeris may be extracted from a longer one 
                   using the program, "binshort.f".
                   

  for non-UNIX users
  --------------

  "asc2eph.f"    : A one-time conversion program which converts the ephemeris 
                   from ASCII format into binary form. 
                           
  "ascSYYYY.XXX" : ASCII ephemeris files from JPL Ephemeris DEXXX, each 
                   covering 20 years, starting in the year SYYYY ("p/m" for 
                   "+/-").  The 20-year blocks may be converted separately 
                   into binary ephemeris files using "asc2eph" or they may 
                   be combined into a single ASCII files and then converted 
                   into a single binary file.  Subsequently, separate binary 
                   files may be merged into a single ephemeris file using
                   the program, "binmerge.f".  Also, a shorter-span ephemeris 
                   may be extracted from a longer one using the program, 
                   "binshort.f".

  "header.XXX" : Header information for ephemeris deXXX, needed by "asc2eph.f".

******************************************************************************

AVAILABLE EPHEMERIDES
---------------------

DE200 : (includes nutations but not librations)

     JED 2305424.5 (1599 DEC 09)  to  JED 2513360.5  (2169 MAR 31)

     This ephemeris has been the basis of the Astronomical Almanac since 
     1984.  It is based upon the dynamical equator and equinox of J2000 
     (see Standish, 1982 and Standish, 1990).

DE403 : (includes both nutations and librations)

     JED 2433264.5 (1949 DEC 14)  to  JED 2469808.5 (2050 JAN 02)

     Tied to the International Celestial Reference Frame through comparison of
     UTPM estimates from Lunar Laser Ranging and VLBI

DE405 : (includes both nutations and librations)

     JED 2305424.50  (1599 DEC 09)  to  JED 2525008.50  (2201 FEB 20)

     Tied to the International Celestial Reference Frame through VLBI
     observations of the Magellan spacecraft in orbit around Venus.
     DE405 was created in May-June 1997.

DE406 : a "Long Ephemeris" (includes neither nutations nor 
        librations)

     JED 0624976.50 (-3001 FEB 04) to 2816912.50 (+3000 MAY 06)

     This is the same ephemeris as DE405, though the accuracy of the 
     interpolating polynomials has been lessened (interpolation on the 
     64-day mesh points remains exact, however).  For DE406/LE406, the 
     interpolating accuracy is no worse than 25 meters for any planet and 
     no worse than 1 meter for the moon.

     DE406 requires about 10 megabytes for each 300-year block.

DE410 : Specialzed ephemeris used for Mars Exploration Rover navigation
        (DE409 is identical except for the reference vale uf GM for Mars)
        Tied to ICRF by VLBI observations of Mars Global Surveyor and
        Mars Odyssey.

      Created  24 April 2003

DE413 : A special ephemeris to update the orbit of Pluto to aid in planning
       for an accolutation of a relatively bright start by Pluto's satellite Charon
       on 11 July 2005.

      Created 4 November 2004

DE414 : An ephemeris fit to ranging data from MGS and Odyssey through 2003
       in addition to many other data types for all planets.
       Integration covers 1599 to 2201. Some aspects are documented in
       Alex S Konopliv et al., Icarys vol 182, pp 23-50 (2006).

       Created May 2005 

******************************************************************************

EPHEMERIS SIZES (ftp blocks)
----------------------------
                                    de200    de403     de405     de406

UNIX binary (50-year blocks)  :     3.8 Mb   4.1 Mb    4.7 Mb

UNIX binary (300-year blocks) :                                  10.0 Mb

ASCII (20-year blocks)        :     5.2 Mb             6.4 Mb

ASCII (25-year blocks)        :              6.1 Mb

******************************************************************************

REFERENCES
----------

 Newhall, X X, Standish, E.M. and Williams, J.G.: 1983, "DE102: a numerically 
   integrated ephemeris of the Moon and planets spanning forty-four centuries",
   Astronomy & Astrophysics, vol. 125, pp. 150-167.

 Standish, E.M.: 1982, "Orientation of the JPL Ephemerides, DE200/LE200, to 
   the Dynamical Equinox of J2000", Astronomy & Astrophysics, vol. 114, 
   pp. 297-302.

 Standish, E.M.: 1990, "The Observational Basis for JPL's DE200, the 
   planetary ephemeris of the Astronomical Almanac", Astronomy & Astrophysics,
   vol. 233, pp. 252-271.

 Standish, E.M., Newhall, X X, Williams, J.G. and Folkner, W.F.: 1995, 
   "JPL Planetary and Lunar Ephemerides, DE403/LE403", JPL IOM 314.10-127.

 Standish,E.M.: 1998, "JPL Planetary and Lunar Ephemerides,
   DE405/LE405",JPL IOM  312.F-98-048. 

   [the preceeding 2 references are available in PostScript on the
   following website: http://ssd.jpl.nasa.gov/iau-comm4/relateds.html]

******************************************************************************

SOFTWARE DESCRIPTION
--------------------

The software package (apart from the programs used to initially construct the
ephemeris, asc2eph, binmerge, and binshort) consists of a main test program 
which uses the reading and interpolating routines in order to retrieve the 
ephemeris data and to compare the results against the Test Data.  

Two of the subroutines called by testeph.f are of primary interest to the user:
"PLEPH" and "SELCON".  Three others, "CONST", "DPLEPH", and "STATE" may also be
useful.

   PLEPH  :  Get the state vector (position and velocity) of one body with 
              respect to another at any given time within the interval covered 
              by the ephemeris.

   SELCON :  Retrieve values of user-requested constants on the ephemeris file.
 
   CONST  :  Retrieve values of all of the constants on the ephemeris file.

   DPLEPH :  Same as PLEPH, but with increased precision in the input time.

   STATE  :  Read and interpolate the ephemeris file. (Called by PLEPH).

******************************************************************************

SOFTWARE USAGE
--------------

C  NOTE : Over the years, different versions of PLEPH have had a 5th argument:
C  sometimes, an error return statement number; sometimes, a logical denoting
C  whether or not the requested date is covered by the ephemeris.  We apologize
C  for this inconsistency; in this version, we use only the four necessary
C  arguments and do the testing outside of the subroutine.


**  PLEPH  ********  subroutine pleph( tdb, npl, nctr, pv)  **********

    Input
    -----
          tdb [d.p.]  : julian ephemeris date
          npl [int.]  : planet number
          nctr [int.] : center number

             identifications for "npl" and "nctr"
             ------------------------------------
              1 = mercury           8 = neptune
              2 = venus             9 = pluto
              3 = earth            10 = moon
              4 = mars             11 = sun
              5 = jupiter          12 = solar-system barycenter
              6 = saturn           13 = earth-moon barycenter
              7 = uranus           14 = nutations in longitude and obliuity
                                   15 = librations (if they exist on the file)
                 (for nutations and librations, nctr=0)
    Output
    ------
          pv(6) [d.p.]  : x,y,z,x-dot,y-dot,z-dot [au, au/day]
                  for nutations, d(psi), d(eps), d(psi)-dot, d(eps)-dot
                                  [rads, rads/day]
                  for librations, (Euler angles and rates, w.r.t. the ephemeris
                                  reference frame)   [rads, rads/day]

**  SELCON  ********  subroutine selcon ( nams, nmbr, vals)  **********

    Input
    -----
    nams(nmbr) [char*6] : names for which values are requested (the list of 
                          names is given below)
    nmbr [int.]         : the number of names in 'nams'

    Output
    ------
    vals(nmbr) [d.p.]   : values corresponding to the names in 'nams'


**  CONST  ********  subroutine const(nmv,vlv,sss,nrv)  **********

    Input   (none)
    -----

    Output
    ------
     nmv(nrv) [char*6] : list of nrv names associated with the values in vlv
     vlv(nrv) [d.p.]   : nrv values associated with the names in nmcc
     sss(3) [d.p.]     :
                            sss(1) : starting jed of the ephemeris file
                            sss(2) : ending jed of the ephemeris file
                            sss(3) : number of days covered by each block
                                     of Chebychev coefficients
     nrv [int.]        : number of values in nmv and vlv


**  STATE  ********  subroutine state(jed,list,pv,nut,*)  **********
           
  [This subroutine is identical to that provided in the past; it is still 
   provided to give previous users compatability; it is not recommended for 
   use by first-time users.]
                                                                     
**  DPLEPH  ********  entry dpleph( tdb2, npl, nctr, pv)  **********

  This entry is identical to "PLEPH", except that the input time, tdb2, is
  doubly-dimensioned for increased precision  [ double precision tdb2(2) ].

                          Any combination of tdb2(1)+tdb2(2) which falls within
                          the time span on the file is a permissible epoch.    
                                                                               
                          For ease in programming, the user may put the entire 
                          date into tdb2(1) and set tdb2(2)=0.        

                          However, for maximum interpolation accuracy, set 
                          tdb2(1) equal to the most recent midnight at or 
                          before interpolation epoch (i.e., xxxxxxx.5d0) and 
                          set tdb2(2) equal to the remaining fractional part 
                          of the day.

                          As an alternative, it may prove convenient to set 
                          tdb2(1) equal to some fixed epoch, such as start of 
                          integration, and set tdb2(2) equal to the remainder 
                          of the desired epoch.

******************************************************************************

CONSTANTS ON THE EPHEMERIS FILE
-------------------------------

The following is a partial list of constants found on the ephemeris file:

  DENUM           Planetary ephemeris number.
  LENUM           Lunar ephemeris number.
  TDATEF, TDATEB  Dates of the Forward and Backward Integrations
  CLIGHT          Speed of light (km/s).
  AU              Number of kilometers per astronomical unit.
  EMRAT           Earth-Moon mass ratio.
  GMi             GM for ith planet [au**3/day**2].
  GMB             GM for the Earth-Moon Barycenter [au**3/day**2].
  GMS             Sun (= k**2) [au**3/day**2].
  X1, ..., ZD9    Initial conditions for the numerical integration,
                  given at "JDEPOC", with respect to "CENTER".
  JDEPOC          Epoch (JED) of initial conditions, normally JED 2440400.5.
  CENTER          Reference center for the initial conditions.
                  (Sun: 11,  Solar System Barycenter: 12)
  RADi            Radius of ith planet [km].
  MA0001...MA0324 GM's of asteroid number 0001 ... 0234 [au**3/day**2].
  PHASE           The phase angle of the moon's rotation.
  LOVENO          The Love Number, k2, for the moon.
  PHI, THT, PSI   Euler angles of the orientation of the lunar mantle.
  OMEGAX, ...     Rotational velocities of the lunar mantle.
  PHIC,THTC,PSIC  Euler angles of the orientation of the lunar core.
  OMGCX, ...      Rotational velocities of the lunar core.

******************************************************************************

ASSISTANCE
----------

If you are really stuck, direct your questions to

*********************************************************
* William Folkner; JPL m/s 301-150; Pasadena, CA  91109 *
* TEL: 818-354-0443                   FAX: 818-393-7631 *
* e-mail: William.Folkner@jpl.nasa.gov                   *
*********************************************************

   Please include your name, address, phone number and e-mail address.

===============================================================================
         APPENDIX A :  The internal format of the ephemeris files

On the first record of an export binary file or in the "GROUP 1050 of the ascii
"header", there are 3 sets of 13 integers each.  (In the binary version, the
13th of each set is stored away from the first 12.)

The 13 triplets give information about the location, order and time-coverage of
the chebychev polynomials corresponding to the following 13 items:

       Mercury
       Venus
       Earth-Moon barycenter
       Mars 
       Jupiter 
       Saturn
       Uranus
       Neptune
       Pluto
       Moon (geocentric)
       Sun
       Nutations
       Librations

Word (1,i) is the starting location in each data record of the chebychev 
coefficients belonging to the ith item.  Word (2,i) is the number of chebychev 
coefficients per component of the ith item, and Word (3,i) is the number of 
complete sets of coefficients in each data record for the ith item.

Data Records ("GROUP 1070")

These records contain the actual ephemeris data in the form of chebychev 
polynomials.

The first two double precision words in each data record contain

         Julian date of earliest data in record.
         Julian date of latest data in record.

The remaining data are chebychev position coefficients for each component of 
each body on the tape.  The chebychev coefficients for the planets represent 
the solar system barycentric positions of the centers of the planetary systems.

There are three cartesian components (x, y, z), for each of the items #1-11; 
there are two components for the 12th item, nutations : d(psi) and d(epsilon);
there are three components for the 13th item, librations : three euler angles.

Planetary positions are given in kilometers.  DE200 is nominally with respect 
to the mean equator and equinox of J2000, coinciding with the origin of FK5; 
DE403 and DE405 are with respect to the Int Cel Ref Frame.  The nutations and 
librations are given in radians.

===============================================================================

         APPENDIX B : CDrom available from Willmann-Bell


The CD's containing DE200 and DE405 (unix and ascii) and also DE406 (unix only)
are available from Willmann-Bell.  The price is about $25 per disk.

         Willmann-Bell, Inc.
         PO Box 35025
         Richmond, VA 23235
         804-320-7016
         804-272-5920 (Fax)

         http://www.willbell.com/software/jpl.htm

There is an order form that calculates all costs to any country in the world 
on their web page (www.willbell.com).  On the home page there is an icon for 
this. The order form activates security for credit card transactions.  If one
does not want to order over the web, one can print the order form and fax or 
mail it to them.

===============================================================================

         APPENDIX C : Corrections to the software on the CDroms

   The enclosed are improvements to the software on the CDroms.
      (The ftp software on "ssd.jpl.nasa.gov" has been updated.)

1)  In "testeph.f" on both CDroms, line 95 should now be

      INTEGER LINE/0/

2)  In "testeph.f" on both CDroms, the statement at line 142 was

        DEL=DABS(R(NCOORD)-XI)/(DABS(R(NCOORD))+DABS(XI))

  it should be replaced by:

        DEL=DABS(R(NCOORD)-XI)
        if(ntarg .eq. 15 .and. ncoord .eq. 3)
       * del=del/(0.23d0*(et-2451545.d0))

3)  In "testeph.f" on both CDroms, the statement that was 
    at line 446 should now be

      IF(FIRST) CALL STATE(0.D0,0,0,0.D0)

4)  In "asc2eph.f" on both CDroms, the statement near line 190 was 

       READ (*,'(2I6,3000(/3D26.18))',IOSTAT =IN)
      .      NRW, NCOEFF, (DB(K),K=1,NCOEFF)

  it should replaced by:

    1  READ(*,'(2i6)')NRW,NCOEFF
       if(NRW .EQ. 0) GO TO 1
       READ (*,'(3D26.18)',IOSTAT =IN) (DB(K),K=1,NCOEFF)

5)  In "asc2eph.f" ***>> on the DE406 ascii CDrom only <<*** :

  "3D26.18" in line 268 should be replaced by "3D23.15"

6)  In "asc2eph.f" on both CDroms, the 4th from last line was

       READ (*, '(A)') BLANK

  it should now be:

       IF(HEADER .NE. 'GROUP   1070') READ (*, '(A)') BLANK


===============================================================================

         APPENDIX D : Documentation of DE405 and DE406

DE405 Documentation is available from the anonymous ftp site, 
"ssd.jpl.nasa.gov", in the directory, "/disk/ftp/pub/eph/export/ascii/", 

The memo is in two forms: 

  1) in a single complete PostScript file, "de405iom.ps", or in

  2) LaTeX file, "de405iom.tex" with the accompanying figures,
     "de405fig1.ps", ... "de405fig8.ps".

The memo for DE403 remains in the directory, "/pub/eph/planets/de403.iom/":
 "de403iom.tex" along with the separate figures, "fig1.ps", ... "fig9.ps".

===============================================================================

Appendix E : Versions of the software in various languages (from outside sources)

There are a number of contributed versions of the reading routines in
various languages; there are also ephemeris toolkits, binary file
conversions, etc.  These are simply listed here; I have not tested
them.

All responsibility for these software packages resides with their
authors, and all questions pertaining to their use must be directed to
the authors themselves. 


C-Versions
----------

From Paul Hardy : http://www.ephemeris.com/software.html

The ephemeris.com Software Library, designed to read, write, and
interpolate NASA's JPL ephemeris files (DE200, DE405, DE406, etc.) is
now available online at

     http://www.ephemeris.com/software.html

The software includes standalone programs to read and write ASCII and
binary ephemeris files, interpolate positions, and verify calculations
with pre-computed data from JPL.  The accuracy should match the
accuracy of JPL's software.  The software is written in C, and should
work on any Unix machine.  It will probably run on any other computer
that has the Gnu C compiler.

This software is designed to directly read the binary and ASCII
ephemeris files on JPL's ephemeris CD (available from
http://www.willbell.com), regardless of the machine's byte ordering. 

-----------------

 From Kourosh Arfa-Kaboodvand, Technical University of Darmstadt:
  (Kourosh@ipgs.ipg.verm.tu-darmstadt.de)

     /ftp/pub/ephem/export/C-versions/kourosh  

 From David Hoffman, Johnson Space Center (david.a.hoffman1@jsc.nasa.gov): 

     /ftp/pub/ephem/export/C-versions/hoffman

-----------------

 From Dr. Piotr Dybczynski, Astronomical Observatory, Poznan, Poland
        (dybol@phys.amu.edu.pl)

      /ftp.astro.amu.edu.pl, IP: 150.254.66.60


FITS-version
------------

 From Arnold Rots of Harvard-Smithsonian Astrophysical Observatory
   (arots@head-cfa.harvard.edu)

    ftp://heasarc.gsfc.nasa.gov/xte/calib_data/clock/bary


Python version
--------------

 From Ray Buvel 

     rbuvel@wi.rr.com


Matlab-Based JPL Solar System Ephemeris Toolbox
-----------------------------------------------

 From Brian Sauser at Kennedy Space Center (brian.sauser-1@ksc.nasa.gov)

     http://technology.ksc.nasa.gov 


UNIX - LINUX Binary Format Conversion
-------------------------------------

 From Thierry Pauwels, Royal Observatory of Belgium:

    Thierry.Pauwels@oma.be
===============================================================================
