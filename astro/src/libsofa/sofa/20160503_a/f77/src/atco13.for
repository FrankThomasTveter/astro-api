      SUBROUTINE iau_ATCO13 ( RC, DC, PR, PD, PX, RV,
     :                        UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,
     :                        PHPA, TC, RH, WL,
     :                        AOB, ZOB, HOB, DOB, ROB, EO, J )
*+
*  - - - - - - - - - - -
*   i a u _ A T C O 1 3
*  - - - - - - - - - - -
*
*  ICRS RA,Dec to observed place.  The caller supplies UTC, site
*  coordinates, ambient air conditions and observing wavelength.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     RC,DC    d      ICRS right ascension at J2000.0 (radians, Note 1)
*     PR       d      RA proper motion (radians/year; Note 2)
*     PD       d      Dec proper motion (radians/year)
*     PX       d      parallax (arcsec)
*     RV       d      radial velocity (km/s, +ve if receding)
*     UTC1     d      UTC as a 2-part...
*     UTC2     d      ...quasi Julian Date (Notes 3-4)
*     DUT1     d      UT1-UTC (seconds, Note 5)
*     ELONG    d      longitude (radians, east +ve, Note 6)
*     PHI      d      latitude (geodetic, radians, Note 6)
*     HM       d      height above ellipsoid (m, geodetic, Notes 6,8)
*     XP,YP    d      polar motion coordinates (radians, Note 7)
*     PHPA     d      pressure at the observer (hPa = mB, Note 8)
*     TC       d      ambient temperature at the observer (deg C)
*     RH       d      relative humidity at the observer (range 0-1)
*     WL       d      wavelength (micrometers, Note 9)
*
*  Returned:
*     AOB      d      observed azimuth (radians: N=0,E=90)
*     ZOB      d      observed zenith distance (radians)
*     HOB      d      observed hour angle (radians)
*     DOB      d      observed declination (radians)
*     ROB      d      observed right ascension (CIO-based, radians)
*     EO       d      equation of the origins (ERA-GST)
*     J        i      status: +1 = dubious year (Note 4)
*                              0 = OK
*                             -1 = unacceptable date
*
*  Notes:
*
*  1)  Star data for an epoch other than J2000.0 (for example from the
*      Hipparcos catalog, which has an epoch of J1991.25) will require
*      a preliminary call to iau_PMSAFE before use.
*
*  2)  The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt.
*
*  3)  UTC1+UTC2 is quasi Julian Date (see Note 2), apportioned in any
*      convenient way between the two arguments, for example where UTC1
*      is the Julian Day Number and UTC2 is the fraction of a day.
*
*      However, JD cannot unambiguously represent UTC during a leap
*      second unless special measures are taken.  The convention in the
*      present routine is that the JD day represents UTC days whether
*      the length is 86399, 86400 or 86401 SI seconds.
*
*      Applications should use the routine iau_DTF2D to convert from
*      calendar date and time of day into 2-part quasi Julian Date, as
*      it implements the leap-second-ambiguity convention just
*      described.
*
*  4)  The warning status "dubious year" flags UTCs that predate the
*      introduction of the time scale or that are too far in the
*      future to be trusted.  See iau_DAT for further details.
*
*  5)  UT1-UTC is tabulated in IERS bulletins.  It increases by exactly
*      one second at the end of each positive UTC leap second,
*      introduced in order to keep UT1-UTC within +/- 0.9s.  n.b. This
*      practice is under review, and in the future UT1-UTC may grow
*      essentially without limit.
*
*  6)  The geographical coordinates are with respect to the WGS84
*      reference ellipsoid.  TAKE CARE WITH THE LONGITUDE SIGN:  the
*      longitude required by the present routine is east-positive
*      (i.e. right-handed), in accordance with geographical convention.
*
*  7)  The polar motion XP,YP can be obtained from IERS bulletins.  The
*      values are the coordinates (in radians) of the Celestial
*      Intermediate Pole with respect to the International Terrestrial
*      Reference System (see IERS Conventions 2003), measured along the
*      meridians 0 and 90 deg west respectively.  For many applications,
*      XP and YP can be set to zero.
*
*  8)  If hm, the height above the ellipsoid of the observing station
*      in meters, is not known but phpa, the pressure in hPa (=mB),
*      is available, an adequate estimate of hm can be obtained from
*      the expression
*
*            hm = -29.3 * tsl * log ( phpa / 1013.25 );
*
*      where tsl is the approximate sea-level air temperature in K
*      (See Astrophysical Quantities, C.W.Allen, 3rd edition, section
*      52).  Similarly, if the pressure phpa is not known, it can be
*      estimated from the height of the observing station, hm, as
*      follows:
*
*            phpa = 1013.25 * exp ( -hm / ( 29.3 * tsl ) );
*
*      Note, however, that the refraction is nearly proportional to
*      the pressure and that an accurate phpa value is important for
*      precise work.
*
*  9)  The argument WL specifies the observing wavelength in
*      micrometers.  The transition from optical to radio is assumed to
*      occur at 100 micrometers (about 3000 GHz).
*
*  10) The accuracy of the result is limited by the corrections for
*      refraction, which use a simple A*tan(z) + B*tan^3(z) model.
*      Providing the meteorological parameters are known accurately and
*      there are no gross local effects, the predicted observed
*      coordinates should be within 0.05 arcsec (optical) or 1 arcsec
*      (radio) for a zenith distance of less than 70 degrees, better
*      than 30 arcsec (optical or radio) at 85 degrees and better than
*      20 arcmin (optical) or 30 arcmin (radio) at the horizon.
*
*      Without refraction, the complementary routines iau_ATCO13 and
*      iau_ATOC13 are self-consistent to better than 1 microarcsecond
*      all over the celestial sphere.  With refraction included,
*      consistency falls off at high zenith distances, but is still
*      better than 0.05 arcsec at 85 degrees.
*
*  11) "Observed" Az,ZD means the position that would be seen by a
*      perfect geodetically aligned theodolite.  (Zenith distance is
*      used rather than altitude in order to reflect the fact that no
*      allowance is made for depression of the horizon.)  This is
*      related to the observed HA,Dec via the standard rotation, using
*      the geodetic latitude (corrected for polar motion), while the
*      observed HA and RA are related simply through the Earth rotation
*      angle and the site longitude.  "Observed" RA,Dec or HA,Dec thus
*      means the position that would be seen by a perfect equatorial
*      with its polar axis aligned to the Earth's axis of rotation.
*
*  12) It is advisable to take great care with units, as even unlikely
*      values of the input parameters are accepted and processed in
*      accordance with the models used.
*
*  Called:
*     iau_APCO13   astrometry parameters, ICRS-observed, 2013
*     iau_ATCIQ    quick ICRS to CIRS
*     iau_ATIOQ    quick CIRS to observed
*
*  This revision:   2016 February 2
*
*  SOFA release 2016-05-03
*
*  Copyright (C) 2016 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION RC, DC, PR, PD, PX, RV, UTC1, UTC2, DUT1,
     :                 ELONG, PHI, HM, XP, YP, PHPA, TC, RH, WL,
     :                 AOB, ZOB, HOB, DOB, ROB, EO
      INTEGER J

      INTEGER JS
      DOUBLE PRECISION ASTROM(30)
      DOUBLE PRECISION RI, DI

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Star-independent astrometry parameters.
      CALL iau_APCO13 ( UTC1, UTC2, DUT1, ELONG, PHI, HM, XP, YP,
     :                  PHPA, TC, RH, WL, ASTROM, EO, JS )
      IF ( JS.LT.0 ) GO TO 9

*  Transform ICRS to CIRS.
      CALL iau_ATCIQ ( RC, DC, PR, PD, PX, RV, ASTROM, RI, DI )

*  Transform CIRS to observed.
      CALL iau_ATIOQ ( RI, DI, ASTROM, AOB, ZOB, HOB, DOB, ROB )

*  Return OK/warning status.
 9    CONTINUE
      J = JS

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2016
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
