      SUBROUTINE iau_C2I06A ( DATE1, DATE2, RC2I )
*+
*  - - - - - - - - - - -
*   i a u _ C 2 I 0 6 A
*  - - - - - - - - - - -
*
*  Form the celestial-to-intermediate matrix for a given date using the
*  IAU 2006 precession and IAU 2000A nutation models.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DATE1,DATE2    d       TT as a 2-part Julian Date (Note 1)
*
*  Returned:
*     RC2I         d(3,3)    celestial-to-intermediate matrix (Note 2)
*
*  Notes:
*
*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TT)=2450123.7 could be expressed in any of these ways,
*     among others:
*
*            DATE1          DATE2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 method is best matched to the way
*     the argument is handled internally and will deliver the
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*
*  2) The matrix RC2I is the first stage in the transformation from
*     celestial to terrestrial coordinates:
*
*        [TRS]  =  RPOM * R_3(ERA) * RC2I * [CRS]
*
*               =  RC2T * [CRS]
*
*     where [CRS] is a vector in the Geocentric Celestial Reference
*     System and [TRS] is a vector in the International Terrestrial
*     Reference System (see IERS Conventions 2003), ERA is the Earth
*     Rotation Angle and RPOM is the polar motion matrix.
*
*  Called:
*     iau_PNM06A   classical NPB matrix, IAU 2006/2000A
*     iau_BPN2XY   extract CIP X,Y coordinates from NPB matrix
*     iau_S06      the CIO locator s, given X,Y, IAU 2006
*     iau_C2IXYS   celestial-to-intermediate matrix, given X,Y and s
*
*  References:
*
*     McCarthy, D. D., Petit, G. (eds.), 2004, IERS Conventions (2003),
*     IERS Technical Note No. 32, BKG
*
*     Capitaine, N. & Wallace, P.T., 2006, Astron.Astrophys. 450, 855
*
*     Wallace, P.T. & Capitaine, N., 2006, Astron.Astrophys. 459, 981
*
*  This revision:  2007 May 11
*
*  SOFA release 2016-05-03
*
*  Copyright (C) 2016 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, RC2I(3,3)

      DOUBLE PRECISION RBPN(3,3), X, Y, S

      DOUBLE PRECISION iau_S06

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Obtain the celestial-to-true matrix (IAU 2006/2000A).
      CALL iau_PNM06A ( DATE1, DATE2, RBPN )

*  Extract the X,Y coordinates.
      CALL iau_BPN2XY ( RBPN, X, Y )

*  Obtain the CIO locator.
      S = iau_S06 ( DATE1, DATE2, X, Y )

*  Form the celestial-to-intermediate matrix.
      CALL iau_C2IXYS ( X, Y, S, RC2I )

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
