      subroutine state()
      implicit none
C     
      integer mjd
      parameter (mjd=100)
      integer njd
      character*30 jd30(mjd)
      real lat,lon,hgt
      character*250 crc250
      integer irc
C
      integer, external :: length
      CHARACTER*1000 :: s1000
      character*1 :: ic         ! ichar of current character
      integer :: spos           ! start of segment
      integer :: ipos           ! equal position
      integer :: epos           ! end of segment
      logical :: bdone          ! are we done?
      integer :: mode
      logical :: lstart,lstop,dstart,dstop,ldefault,lid
      integer :: cseq,oseq,idebug,lens,lenc
      integer :: yy1,mm1,dd1,hh1,mi1,yy2,mm2,dd2,hh2,mi2
      integer :: yy,mm,dd,hh,mi
      integer :: values(8)
      integer :: ii, jj,istart,istop,nval
      real :: sec1,sec2,dsec1,dsec2,sec
      logical :: debug, bdeb
C
      bdeb=.false.
      CALL get_environment_variable("QUERY_STRING", s1000)
      lens=len(trim(s1000))
C
C     make sure query_string ends with ';'
C
      lens=min(1000,lens+1)
      s1000(lens:lens)=';'
C
C     &lat=60.0;lon=0.0;hgt=0.0;dtg=2010-10-10T10:10:10Z;
C
      if (bdeb) write(*,*)'Query: "',s1000(1:lens)//'"'
      lat=0.0D0
      lon=0.0D0
      hgt=0.0D0
      njd=0
      ii=1
      mode=0                    ! initial
      bdone=(ii.gt.lens)
      do while (.not. bdone)
         if (mode.eq.0) then
            if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then
               ii=ii+1
            else if (s1000(ii:min(1000,ii+2)).eq.'lat') then
               if (bdeb) write(*,*) 'Found "lat"',ii,mode
               mode=1           ! search for latitude
               ii=ii+3
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+2)).eq.'lon') then
               if (bdeb) write(*,*) 'Found "lon"',ii,mode
               mode=2           ! search for longitude
               ii=ii+3
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+2)).eq.'hgt') then
               if (bdeb) write(*,*) 'Found "hgt"',ii,mode
               mode=3           ! search for height
               ii=ii+3
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+2)).eq.'dtg') then
               if (bdeb) write(*,*) 'Found "dtg"',ii,mode
               mode=4           ! search for height
               ii=ii+3
               istart=ii
               istop=istart-1
            else ! strange character
               ii=ii+1
            end if
         else if (mode.eq.1) then ! search for lat
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';') then ! we are done
               if (bdeb) write(*,*) 'Found lat: "'//
     &              s1000(istart:istop)//'"',ii,mode,nval
               read(s1000(istart:istop),*,iostat=irc) 
     &              lat
               if (irc.ne.0) then
                  irc=0
               end if
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.2) then ! search for lon
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';') then ! we are done
               if (bdeb) write(*,*) 'Found lon: "'//
     &              s1000(istart:istop)//'"',ii,mode,nval
               read(s1000(istart:istop),*,iostat=irc) 
     &              lon
               if (irc.ne.0) then
                  irc=0
               end if
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.3) then ! search for hgt
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';') then ! we are done
               if (bdeb) write(*,*) 'Found hgt: "'//
     &              s1000(istart:istop)//'"',ii,mode,nval
               read(s1000(istart:istop),*,iostat=irc) 
     &              hgt
               if (irc.ne.0) then
                  irc=0
               end if
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.4) then ! search time
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';') then ! we are done
               if (bdeb) write(*,*) 'Found time: "'//
     &              s1000(istart:istop)//'"',ii,mode
               njd=min(mjd,njd+1)
               jd30(njd)=s1000(istart:istop)
               ii=ii+1
               mode=0
C$$$     read(s1000(istart:istop),*,iostat=irc) 
C$$$     &              yy,mm,dd,hh,mi,sec
c$$$  ! call date2jd(jd(njd),yy1,mm1,dd1,hh1,mi1,sec1)
            else
c$$$  if (s1000(ii:ii).eq.'-'.or.s1000(ii:ii).eq.':'.or.
c$$$  &              s1000(ii:ii).eq.'T'.or.s1000(ii:ii).eq.'Z') then
c$$$  s1000(ii:ii)=' '
c$$$  end if
               istop=ii
               ii=ii+1
            end if
         else
            ii=ii+1
         end if
         bdone=(ii.gt.lens)
      end do

      call astroState(njd,JD30,lat,lon,hgt,crc250,irc)
      if (irc.ne.0) then
         write(*,'("Content-type: text/html;charset=iso-8859-1")')
         write(*,'("")')
         write(*,'(A)') '<?xml version="1.0" encoding="utf-8"?>'
         call chop0(crc250,250)
         lenc=length(crc250,250,10)
         write(*,'(A,I0,A,A,A)') '<Error irc="',irc,
     &        '" msg="',crc250(1:lenc),'"/>"'
      end if
      return
      end subroutine state
