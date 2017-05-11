      program test
      character*1024 :: file = "../../eph/JPLEPH.405"
      character*250 :: crc250
      integer :: irc
      call jplephOpen(file,crc250,irc)
      call event()
!      call state()
      call jplephClose()
      end program test
