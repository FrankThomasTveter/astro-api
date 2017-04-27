      SUBROUTINE DJ2000(DAY,YY,MM,DD,HH,MI,SEC)
!P  COMPUTES CALENDER DATE FROM MODIFIED JULIAN DAY 2000
!   VALID FOR DATES BETWEEN 1950/JAN/1 AND 2099/DEC/31.
!
!   MJD(2000) = MJD(1950) - 18262.0 IS = 0 ON 2000/01/01 AT 00:00:00.
!
!I  (REAL) DAY = MOD. JUL. DAY, REFERRED TO 2000.
!O  (INT*4) YY = YEAR WITH 4 DIGITS
!O  (INT*4) MM = MONTH
!O  (INT*4) DD = DAY
!O  (INT*4) HH = HOUR
!O  (INT*4) MI = MINUTE
!O  (REAL) SEC = SECOND.
!
      IMPLICIT NONE
      REAL DAY, SEC
      INTEGER YY,MM,DD,HH,MI
      INTEGER irc
      real day0,day1,fd

      day0=floor(day)
      day1=day-day0
      day0=day0 + 2451544.5D0

      call  iau_JD2CAL ( day0, day1, YY, MM, DD, FD, IRC )
      
      sec=fd*86400.0D0
      HH=int(sec/3600.0D0)
      sec=sec-real(hh)*3600.0D0
      MI=int(sec/60.0D0)
      sec=sec-real(MI)*60.0D0

C      write(*,*) 'DJ2000:', yy,mm,dd,hh,mi,sec,day+2451544.5D0

      RETURN
      END
