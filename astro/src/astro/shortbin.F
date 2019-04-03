      PROGRAM SHORTBIN
C     
C     SHORTBIN PROGRAM FOR DATA TEST READ
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




      s1000='?lat=74&lon=56&date=2018-06-24&offset=+03:00&days=3'
      




      CALL SHORT(s1000,   ! query string
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250)
C
      do ii=1,nline
         write(*,*) line250(ii)(1:lenline(ii))
      end do
      

      END
