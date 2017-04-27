      SUBROUTINE JD2DATE (JD, YEAR,MONTH,DAY,HOUR,MINUTES,SECONDS)
C     (corresponds to DJ2000)
C     Computes gregorian (civil) calendar from julian day
C     I  (REAL*8) JD = JULIAN. DAY
C     O  (INT*4) YY = YEAR
C     O  (INT*4) MM = MONTH
C     O  (INT*4) DD = DAY
C     O  (INT*4) HH = HOUR
C     O  (INT*4) MI = MINUTE
C     O  (REAL*8) SEC = SECOND.
C
      implicit none
      REAL*8 JD
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTES
      REAL SECONDS
      REAL DJ
      INTEGER*8 I,J,K,L,M,N,IJ
      IJ= INT(JD+0.5D0)
      DJ=(JD+0.5D0)-real(ij)
      L= IJ+68569
      N= 4*L/146097
      L= L-(146097*N+3)/4
      I= 4000*(L+1)/1461001
      L= L-1461*I/4+31
      J= 80*L/2447
      K= L-2447*J/80
      L= J/11
      J= J+2-12*L
      I= 100*(N-49)+I+L
      YEAR= I
      MONTH= J
      DAY= K
      DJ=DJ*86400.0D0 ! seconds
      HOUR=int(DJ/3600.0D0)
      Dj=DJ-HOUR*3600.0D0
      MINUTES=INT(DJ/60.0D0)
      Dj=DJ-MINUTES*60.0D0
      SECONDS=DJ
      RETURN
      END
