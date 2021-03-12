      subroutine short(s1000,   ! query string
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250)             ! output string
C
      implicit none
C     
      CHARACTER*1000 :: s1000
      integer :: maxline
      integer :: nline
      integer :: lenline(maxline)
      character*250 :: line250(maxline)
C
      character*10 :: lat10,lon10,hgt10,day10,dat10
      character*6 :: off6
C
      integer, external :: length
      character*1 :: ic         ! ichar of current character
      logical :: bdone          ! are we done?
      integer :: mode
      integer :: lens
      integer :: ii, jj,istart,istop,nval
      logical :: bdeb
      data bdeb /.false./
C
C      bdeb=.true.
C     call chop0(s1000,1000)    ! s1000 is already char(0) terminated
      lens=length(s1000,1000,50)
C
C     make sure query_string ends with ';'
C
      lens=min(1000,lens+1)
      s1000(lens:lens)=';'
      if (bdeb) write(*,*)'Query: "',s1000(1:lens)//'"',lens
C
      nline=0
      lat10="60.0"
      lon10="10.0"
      hgt10="0.0"
      day10="0"
      dat10="2020-01-01"
      off6="+00:00"
C
      ii=1
      mode=1                    ! initial
      bdone=(ii.gt.lens)
      do while (.not. bdone)
         if (s1000(ii:ii).eq.'%') then ! special character
            s1000(ii:ii+2)='   '
         end if
         if (mode.eq.1) then
            if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then
               ii=ii+1
            else if (s1000(ii:min(1000,ii+2)).eq.'lat') then
               if (bdeb) write(*,*) 'Found "short"',ii,mode
               mode=2           ! search for seq or type
               ii=ii+3
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+2)).eq.'lon') then
               if (bdeb) write(*,*) 'Found "short"',ii,mode
               mode=3           ! search for seq or type
               ii=ii+3
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+2)).eq.'hgt') then
               if (bdeb) write(*,*) 'Found "short"',ii,mode
               mode=4           ! search for seq or type
               ii=ii+3
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+3)).eq.'days') then
               if (bdeb) write(*,*) 'Found "short"',ii,mode
               mode=5           ! search for seq or type
               ii=ii+4
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+3)).eq.'date') then
               if (bdeb) write(*,*) 'Found "short"',ii,mode
               mode=6           ! search for seq or type
               ii=ii+4
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+5)).eq.'offset') then
               if (bdeb) write(*,*) 'Found "short"',ii,mode
               mode=7           ! search for seq or type
               ii=ii+4
               istart=ii
               istop=istart-1
            else ! strange character
               ii=ii+1
            end if
         else
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
               if (bdeb) write(*,*) 'Found: "'//
     &              s1000(istart:istop)//'"',ii,mode
               if (mode.eq.2) then ! lat
                  lat10=s1000(istart:istop)
               else if (mode.eq.3) then ! lon
                  lon10=s1000(istart:istop)
               else if (mode.eq.4) then ! hgt
                  hgt10=s1000(istart:istop)
               else if (mode.eq.5) then ! days
                  day10=s1000(istart:istop)
               else if (mode.eq.6) then ! date
                  dat10=s1000(istart:istop)
               else if (mode.eq.7) then ! offset
                  off6=s1000(istart:istop)
               end if
               ii=ii+1
               mode=1
            else
               istop=ii
               ii=ii+1
            end if
         end if
         bdone=(ii.gt.lens)
      end do

      call astroShort(lat10,lon10,hgt10,day10,dat10,off6, ! query string
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250)             ! output string
      
      if (bdeb) write(*,*) 'Done...',nline
      return
      end subroutine short
