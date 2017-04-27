      SUBROUTINE iau_C2TPE ( TTA, TTB, UTA, UTB, DPSI, DEPS, XP, YP,
     :                       RC2T )
*+
*  - - - - - - - - - -
*   i a u _ C 2 T P E
*  - - - - - - - - - -
*
*  Form the celestial to terrestrial matrix given the date, the UT1, the
*  nutation and the polar motion.  IAU 2000.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     TTA,TTB     d       TT as a 2-part Julian Date (Note 1)
*     UTA,UTB     d       UT1 as a 2-part Julian Date (Note 1)
*     DPSI,DEPS   d       nutation (Note 2)
*     XP,YP       d       coordinates of the pole (radians, Note 3)
*
*  Returned:
*     RC2T      d(3,3)    celestial-to-terrestrial matrix (Note 4)
*
*  Notes:
*
*  1) The TT and UT1 dates TTA+TTB and UTA+UTB are Julian Dates,
*     apportioned in any convenient way between the arguments UTA and
*     UTB.  For example, JD(UT1)=2450123.7 could be expressed in any of
*     these ways, among others:
*
*             UTA            UTB
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution is
*     acceptable.  The J2000 and MJD methods are good compromises
*     between resolution and convenience.  In the case of UTA,UTB, the
*     date & time method is best matched to the Earth rotation angle
*     algorithm used:  maximum accuracy (or, at least, minimum noise) is
*     delivered when the UTA argument is for 0hrs UT1 on the day in
*     question and the UTB argument lies in the range 0 to 1, or vice
*     versa.
*
*  2) The caller is responsible for providing the nutation components;
*     they are in longitude and obliquity, in radians and are with
*     respect to the equinox and ecliptic of date.  For high-accuracy
*     applications, free core nutation should be included as well as
*     any other relevant corrections to the position of the CIP.
*
*  3) XP and YP are the coordinates (in radians) of the Celestial
*     Intermediate Pole with respect to the International Terrestrial
*     Reference System (see IERS Conventions 2003), measured along the
*     meridians to 0 and 90 deg west respectively.
*
*  4) The matrix RC2T transforms from celestial to terrestrial
*     coordinates:
*
*        [TRS]  =  RPOM * R_3(GST) * RBPN * [CRS]
*
*               =  RC2T * [CRS]
*
*     where [CRS] is a vector in the Geocentric Celestial Reference
*     System and [TRS] is a vector in the International Terrestrial
*     Reference System (see IERS Conventions 2003), RBPN is the
*     bias-precession-nutation matrix, GST is the Greenwich (apparent)
*     Sidereal Time and RPOM is the polar motion matrix.
*
*  5) Although its name does not include "00", this routine is in fact
*     specific to the IAU 2000 models.
*
*  Called:
*     iau_PN00     bias/precession/nutation results, IAU 2000
*     iau_GMST00   Greenwich mean sidereal time, IAU 2000
*     iau_SP00     the TIO locator s', IERS 2000
*     iau_EE00     equation of the equinoxes, IAU 2000
*     iau_POM00    polar motion matrix
*     iau_C2TEQX   form equinox-based celestial-to-terrestrial matrix
*
*  Reference:
*
*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
*     IERS Technical Note No. 32, BKG (2004)
*
*  This revision:  2009 April 1
*
*  SOFA release 2016-05-03
*
*  Copyright (C) 2016 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION TTA, TTB, UTA, UTB, DPSI, DEPS, XP, YP, RC2T(3,3)

      DOUBLE PRECISION EPSA, RB(3,3), RP(3,3), RBP(3,3), RN(3,3),
     :                 RBPN(3,3), GMST, EE, SP, RPOM(3,3)

      DOUBLE PRECISION iau_GMST00, iau_EE00, iau_SP00

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


*  Form the celestial-to-true matrix for this TT.
      CALL iau_PN00 ( TTA, TTB, DPSI, DEPS,
     :                EPSA, RB, RP, RBP, RN, RBPN )

*  Predict the Greenwich Mean Sidereal Time for this UT1 and TT.
      GMST = iau_GMST00 ( UTA, UTB, TTA, TTB )

*  Predict the equation of the equinoxes given TT and nutation.
      EE = iau_EE00 ( TTA, TTB, EPSA, DPSI)

*  Estimate s'.
      SP = iau_SP00 ( TTA, TTB)

*  Form the polar motion matrix.
      CALL iau_POM00 ( XP, YP, SP, RPOM )

*  Combine to form the celestial-to-terrestrial matrix.
      CALL iau_C2TEQX ( RBPN, GMST+EE, RPOM, RC2T )

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
