      LOGICAL FUNCTION DIGIT(STRING)
C     ***********************************************************************
C     +                                                                     *
C     CHECKS IF STRING IS A DIGIT                                           *
C     +                                                                     *
CI    STRING (C*1) = STRING TO BE EXAMINED                                  *
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE   |                 REASON                  | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 18/07/95 | NEW                                     | *
C     |              |          |                                         | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
      IMPLICIT NONE
C
      CHARACTER*1 STRING
C
      LOGICAL BUFF
C
      BUFF= (  STRING.EQ.'0' .OR.
     &     STRING.EQ.'1' .OR.
     &     STRING.EQ.'2' .OR.
     &     STRING.EQ.'3' .OR.
     &     STRING.EQ.'4' .OR.
     &     STRING.EQ.'5' .OR.
     &     STRING.EQ.'6' .OR.
     &     STRING.EQ.'7' .OR.
     &     STRING.EQ.'8' .OR.
     &     STRING.EQ.'9')
C
      DIGIT=BUFF
C
      RETURN
      END
