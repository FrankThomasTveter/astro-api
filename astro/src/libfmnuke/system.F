      integer function system(char250)
      implicit none
      character*250 char250
      write(*,*) 'SYSTEM not available.'
      system=999
      return
      end function system
