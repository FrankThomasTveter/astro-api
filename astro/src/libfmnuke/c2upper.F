      SUBROUTINE C2Upper ( ChString )
      IMPLICIT NONE
c-----------------------------------------
c identification variable for "what"
      character*62 sccsid
c
      data sccsid
     &/'@(#) c2upper.F version 1.1 on 2/12/98'/
c
C
C Purpose:
C   Convert character string to upper case
C
C Method:
C   Loop through each character in ChString.
C   Convert any [a-z] to [A-Z]   (without assuming ASCII)
C
C Current Code Owner: R.J.Renshaw
C
C History:
C Version  Date      Comment
C
C  1.0      27/11/96  Original code. (R.J.Renshaw)
C
C Code Description:
C   FORTRAN 77, following AAPP standards
C
C Declarations:
C
C Global variables:
C
C Subroutine arguments
C   Scalar arguments with intent(InOut):
      CHARACTER*(*) ChString      ! character string
C
C Local Scalars:
      INTEGER       i             ! pointer that loops through ChString
      INTEGER       len_string    ! length of ChString
      CHARACTER*1   c             ! char in ChString pointed to by i
C
C- End of header
C
C     loop through ChString, converting lowercase characters
C
      len_string = LEN ( ChString )
      DO i = 1, len_string
C
C        Get the character pointed to by i
         c = ChString(i:i)
C
C        Is it a lower-case character ?   If so, convert to upper-case
         IF     ( c .EQ. "a" ) THEN
            ChString(i:i) = "A"
         ELSEIF ( c .EQ. "b" ) THEN
            ChString(i:i) = "B"
         ELSEIF ( c .EQ. "c" ) THEN
            ChString(i:i) = "C"
         ELSEIF ( c .EQ. "d" ) THEN
            ChString(i:i) = "D"
         ELSEIF ( c .EQ. "e" ) THEN
            ChString(i:i) = "E"
         ELSEIF ( c .EQ. "f" ) THEN
            ChString(i:i) = "F"
         ELSEIF ( c .EQ. "g" ) THEN
            ChString(i:i) = "G"
         ELSEIF ( c .EQ. "h" ) THEN
            ChString(i:i) = "H"
         ELSEIF ( c .EQ. "i" ) THEN
            ChString(i:i) = "I"
         ELSEIF ( c .EQ. "j" ) THEN
            ChString(i:i) = "J"
         ELSEIF ( c .EQ. "k" ) THEN
            ChString(i:i) = "K"
         ELSEIF ( c .EQ. "l" ) THEN
            ChString(i:i) = "L"
         ELSEIF ( c .EQ. "m" ) THEN
            ChString(i:i) = "M"
         ELSEIF ( c .EQ. "n" ) THEN
            ChString(i:i) = "N"
         ELSEIF ( c .EQ. "o" ) THEN
            ChString(i:i) = "O"
         ELSEIF ( c .EQ. "p" ) THEN
            ChString(i:i) = "P"
         ELSEIF ( c .EQ. "q" ) THEN
            ChString(i:i) = "Q"
         ELSEIF ( c .EQ. "r" ) THEN
            ChString(i:i) = "R"
         ELSEIF ( c .EQ. "s" ) THEN
            ChString(i:i) = "S"
         ELSEIF ( c .EQ. "t" ) THEN
            ChString(i:i) = "T"
         ELSEIF ( c .EQ. "u" ) THEN
            ChString(i:i) = "U"
         ELSEIF ( c .EQ. "v" ) THEN
            ChString(i:i) = "V"
         ELSEIF ( c .EQ. "w" ) THEN
            ChString(i:i) = "W"
         ELSEIF ( c .EQ. "x" ) THEN
            ChString(i:i) = "X"
         ELSEIF ( c .EQ. "y" ) THEN
            ChString(i:i) = "Y"
         ELSEIF ( c .EQ. "z" ) THEN
            ChString(i:i) = "Z"
         ENDIF  ! c
C
      ENDDO  ! i
C
      RETURN
      END
