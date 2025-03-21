      SUBROUTINE iau_G2ICRS ( DL, DB, DR, DD )
*+
*  - - - - - - - - - - -
*   i a u _ G 2 I C R S
*  - - - - - - - - - - -
*
*  Transformation from Galactic Coordinates to ICRS.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DL       d      galactic longitude (radians)
*     DB       d      galactic latitude (radians)
*
*  Returned:
*     DR       d      ICRS right ascension (radians)
*     DD       d      ICRS declination (radians)
*
*  Notes:
*
*  1) The IAU 1958 system of Galactic coordinates was defined with
*     respect to the now obsolete reference system FK4 B1950.0.  When
*     interpreting the system in a modern context, several factors have
*     to be taken into account:
*
*     . The inclusion in FK4 positions of the E-terms of aberration.
*
*     . The distortion of the FK4 proper motion system by differential
*       Galactic rotation.
*
*     . The use of the B1950.0 equinox rather than the now-standard
*       J2000.0.
*
*     . The frame bias between ICRS and the J2000.0 mean place system.
*
*     The Hipparcos Catalogue (Perryman & ESA 1997) provides a rotation
*     matrix that transforms directly between ICRS and Galactic
*     coordinates with the above factors taken into account.  The
*     matrix is derived from three angles, namely the ICRS coordinates
*     of the Galactic pole and the longitude of the ascending node of
*     the galactic equator on the ICRS equator.  They are given in
*     degrees to five decimal places and for canonical purposes are
*     regarded as exact.  In the Hipparcos Catalogue the matrix elements
*     are given to 10 decimal places (about 20 microarcsec).  In the
*     present SOFA routine the matrix elements have been recomputed from
*     the canonical three angles and are given to 30 decimal places.
*
*  2) The inverse transformation is performed by the routine iau_ICRS2G.
*
*  Called:
*     iau_ANP      normalize angle into range 0 to 2pi
*     iau_ANPM     normalize angle into range +/- pi
*     iau_S2C      spherical coordinates to unit vector
*     iau_TRXP     product of transpose of r-matrix and p-vector
*     iau_C2S      p-vector to spherical
*
*  Reference:
*     Perryman M.A.C. & ESA, 1997, ESA SP-1200, The Hipparcos and Tycho
*     catalogues.  Astrometric and photometric star catalogues
*     derived from the ESA Hipparcos Space Astrometry Mission.  ESA
*     Publications Division, Noordwijk, Netherlands.
*
*  This revision:   2015 January 9
*
*  SOFA release 2016-05-03
*
*  Copyright (C) 2016 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION DL, DB, DR, DD

      DOUBLE PRECISION iau_ANP, iau_ANPM

      DOUBLE PRECISION V1(3), V2(3)

*
*  L2,B2 system of galactic coordinates in the form presented in the
*  Hipparcos Catalogue.  In degrees:
*
*  P = 192.85948    right ascension of the Galactic north pole in ICRS
*  Q =  27.12825    declination of the Galactic north pole in ICRS
*  R =  32.93192    longitude of the ascending node of the Galactic
*                   plane on the ICRS equator
*
*  ICRS to galactic rotation matrix, obtained by computing
*  R_3(-R) R_1(pi/2-Q) R_3(pi/2+P) to the full precision shown:
*
      DOUBLE PRECISION R(3,3)
      DATA R(1,1), R(1,2), R(1,3),
     :     R(2,1), R(2,2), R(2,3),
     :     R(3,1), R(3,2), R(3,3) /
     :    -0.054875560416215368492398900454D0,
     :    -0.873437090234885048760383168409D0,
     :    -0.483835015548713226831774175116D0,
     :    +0.494109427875583673525222371358D0,
     :    -0.444829629960011178146614061616D0,
     :    +0.746982244497218890527388004556D0,
     :    -0.867666149019004701181616534570D0,
     :    -0.198076373431201528180486091412D0,
     :    +0.455983776175066922272100478348D0 /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Spherical to Cartesian.
      CALL iau_S2C ( DL, DB, V1 )

*  Galactic to ICRS.
      CALL iau_TRXP ( R, V1, V2 )

*  Cartesian to spherical.
      CALL iau_C2S ( V2, DR, DD )

*  Express in conventional ranges.
      DR = iau_ANP ( DR )
      DD = iau_ANPM ( DD )

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
