      PROGRAM SMALLBIN
C     
C     SMALLBIN PROGRAM FOR DATA TEST READ
C
      IMPLICIT NONE
      CHARACTER*1000 :: s1000
      integer,parameter :: maxline=100
      integer :: nline
      integer :: lenline(maxline)
      character*250 :: line250(maxline)
      integer :: II


      character*1024 :: jpl1024
      character*250 :: crc250,buff250
      integer :: irc,lenc
      integer, external :: length
C
      jpl1024="../../eph/JPLEPH.405"
      call JPLephOpen(jpl1024,crc250,irc) ! open JPL ephemeride file for reading
      if (irc.ne.0) then
         write(*,*) crc250(1:lenc)
         stop("JPL-error")
      end if
C
C      s1000="lat=65.745&lon=10.0&days=4;date=2020-06-19&offset=+01:00;"
      s1000="lat=35.745&lon=10.0&days=4;date=2020-06-19&offset=+01:00;"
      CALL SMALL(s1000,         ! query string
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250)
C
      do ii=1,nline
         write(*,*) line250(ii)(1:lenline(ii))
      end do
C
      END
