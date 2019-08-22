      subroutine state(s1000,   ! query string
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250)
C
      implicit none
C
      CHARACTER*1000 :: s1000
      integer :: maxline
      integer :: nline
      integer :: lenline(maxline)
      character*250 :: line250(maxline)
C     
      integer mjd
      parameter (mjd=1000)
      integer njd
      character*30 jd30(mjd)
      real lat,lon,hgt,stp,dur,dt,jd,jd0
      character*250 crc250
      integer irc
C
      integer, external :: length
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
      character*1 :: c1
      character*30 :: b30
C
      bdeb=.false.
C     call chop0(s1000,1000) ! s1000 is already char(0)-terminated
      lens=length(s1000,1000,50)
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
      stp=0.0D0
      dur=0.0D0
      njd=0
      ii=1
      mode=0                    ! initial
      bdone=(ii.gt.lens)
      do while (.not. bdone)
         if (s1000(ii:ii).eq.'%') then ! special character
            s1000(ii:ii+2)='   '
         end if
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
               mode=4           ! search for dtg
               ii=ii+3
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+2)).eq.'dur') then
               if (bdeb) write(*,*) 'Found "dur"',ii,mode
               mode=5           ! search for per
               ii=ii+3
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+2)).eq.'stp') then
               if (bdeb) write(*,*) 'Found "stp"',ii,mode
               mode=6           ! search for stp
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
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
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
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
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
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
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
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
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
         else if (mode.eq.5) then ! search for dur
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
               if (bdeb) write(*,*) 'Found dur: "'//
     &              s1000(istart:istop)//'"',ii,mode,nval
               read(s1000(istart:istop),*,iostat=irc) 
     &              dur
               if (irc.ne.0) then
                  irc=0
               end if
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.6) then ! search for stp
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
               if (bdeb) write(*,*) 'Found stp: "'//
     &              s1000(istart:istop)//'"',ii,mode,nval
               read(s1000(istart:istop),*,iostat=irc) 
     &              stp
               if (irc.ne.0) then
                  irc=0
               end if
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         else
            ii=ii+1
         end if
         bdone=(ii.gt.lens)
      end do
      !write(*,*)'Stp/Dur:',stp,dur
      if (stp.gt.0.0D0 .and. dur.gt.0.0D0.and.njd.gt.0) then
         b30=jd30(1)
         do ii=1,30
            c1=b30(ii:ii)
            if (c1.ne.'.'.and.
     &           c1.ne.'1'.and.
     &           c1.ne.'2'.and.
     &           c1.ne.'3'.and.
     &           c1.ne.'4'.and.
     &           c1.ne.'5'.and.
     &           c1.ne.'6'.and.
     &           c1.ne.'7'.and.
     &           c1.ne.'8'.and.
     &           c1.ne.'9'.and.
     &           c1.ne.'0') b30(ii:ii)=' '
         end do
         read(b30,*,iostat=irc)yy,mm,dd,hh,mi,sec
         if (irc.ne.0) then
            write(crc250,'(A," Unable to read date from:",A,":",A)')
     &           b30,jd30(1)
            return
         end if
         CALL DATE2JD(JD0,YY,MM,DD,HH,MI,SEC) 
         dt=stp
         !write(*,*)'DJ0:',yy,mm,dd,hh,mi,sec,njd,mjd,stp,dur
         do while (dt.lt.dur.and.njd.lt.mjd)
            njd=njd+1
            dt=dt+stp
            jd=jd0+dt
            CALL JD2DATE(JD,YY,MM,DD,HH,MI,SEC) 
            write(jd30(njd),100) yy,mm,dd,hh,mi,int(sec)
            !write(*,*)"Date:",jd30(njd),njd
         end do
      end if
 100  FORMAT(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,"Z")
      irc=0
      crc250=""
      call astroState(njd,JD30,lat,lon,hgt,
     &     maxline,             ! maximum number of output lines
     &     nline,               ! number of output lines
     &     lenline,             ! length of each out250
     &     line250,crc250,irc)
      if (irc.ne.0) then
         nline=0
         nline=min(maxline,nline+1)
         write(line250(nline),'(A)')
     &        "Content-type: text/xml;" ! "charset=iso-8859-1"
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(A)')" "
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(A)')
     &        '<?xml version="1.0" encoding="utf-8"?>'
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(A)')
     &        '<solarsystem xmlns:xsi='//
     &        '"http://www.w3.org/2001/XMLSchema-instance"'//
     &        ' xmlns="http://astro.met.no"  xsi:schemaLocation='//
     &        '"http://astro.met.no/astro state.xsd">'
         lenline(nline)=len(trim(line250(nline)))
         !nline=min(maxline,nline+1)
         !write(line250(nline),'(A)')" "
         !lenline(nline)=len(trim(line250(nline)))
         call clean0(crc250,250)
         call chop0(crc250,250)
         lenc=length(crc250,250,10)
         nline=min(maxline,nline+1)
         write(line250(nline),'(A,I0,A,A,A)') 
     &        ' <Error irc="',irc,
     &        '" msg="',crc250(1:min(200,lenc)),'"/>'
         lenline(nline)=len(trim(line250(nline)))
         nline=min(maxline,nline+1)
         write(line250(nline),'(A)') '</solarsystem>'
         lenline(nline)=len(trim(line250(nline)))
      end if
      return
      end subroutine state
