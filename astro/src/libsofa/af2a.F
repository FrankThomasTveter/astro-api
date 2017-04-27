      SUBROUTINE iau_AF2A ( S, IDEG, IAMIN, ASEC, RAD, J )
*+
*  - - - - - - - - -
*   i a u _ A F 2 A
*  - - - - - - - - -
*
*  Convert degrees, arcminutes, arcseconds to radians.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     S          c      sign:  '-' = negative, otherwise positive
*     IDEG       i      degrees
*     IAMIN      i      arcminutes
*     ASEC       d      arcseconds
*
*  Returned:
*     RAD        d      angle in radians
*     J          i      status:  0 = OK
*                                1 = IDEG outside range 0-359
*                                2 = IAMIN outside range 0-59
*                                3 = ASEC outside range 0-59.999...
*
*  Notes:
*
*  1)  If the s argument is a string, only the leftmost character is
*      used and no warning status is provided.
*
*  2)  The result is computed even if any of the range checks fail.
*
*  3)  Negative IDEG, IAMIN and/or ASEC produce a warning status, but
*      the absolute value is used in the conversion.
*
*  4)  If there are multiple errors, the status value reflects only the
*      first, the smallest taking precedence.
*
*
*  This revision:  2013 December 2
*
*  SOFA release 2016-05-03
*
*  Copyright (C) 2016 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER S
      INTEGER IDEG, IAMIN
      DOUBLE PRECISION ASEC, RAD
      INTEGER J

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

      DOUBLE PRECISION W

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Preset the status.
      J = 0

*  Validate arcseconds, arcminutes, degrees.
      IF ( ASEC.LT.0D0 .OR. ASEC.GE.60D0 ) J=3
      IF ( IAMIN.LT.0 .OR. IAMIN.GT.59 ) J=2
      IF ( IDEG.LT.0 .OR. IDEG.GT.359 ) J=1

*  Compute the angle.
      W = ( 60D0 * ( 60D0 * DBLE( ABS(IDEG) ) +
     :                      DBLE( ABS(IAMIN) ) ) +
     :                      ABS(ASEC) ) * DAS2R

*  Apply the sign.
      IF ( S .EQ. '-' ) W = -W

*  Return the result.
      RAD = W

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
