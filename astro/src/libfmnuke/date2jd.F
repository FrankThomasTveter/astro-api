      subroutine DATE2JD (JD, YEAR,MONTH,DAY,HOUR,MINUTES,SECONDS)
C     (corresponds to JD2000)
C     Computes julian day from gregorian (civil) calendar
C     O  (REAL*8) JD = JULIAN. DAY
C     I  (INT*4) YY = YEAR
C     I  (INT*4) MM = MONTH
C     I  (INT*4) DD = DAY
C     I  (INT*4) HH = HOUR
C     I  (INT*4) MI = MINUTE
C     I  (REAL*8) SEC = SECOND.
      implicit none
      REAL*8 JD
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTES,I,J,K
      REAL SECONDS
      I= YEAR
      J= MONTH
      K= DAY
      JD= K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12)
     2     /12-3*((I+4900+(J-14)/12)/100)/4
      JD=JD+((HOUR-12.0D0)/24.0D0)+
     &     (MINUTES/1440.0D0)+(SECONDS/86400.0D0)
      RETURN
      END
