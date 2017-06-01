      subroutine event(s1000,   ! query string
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
      real tstartJD             ! start time (in julianDay)
      integer searchCode        ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
      real tstopJD               ! report all events end time (in julianDay)
      integer eventId           ! requested event id (SEE TABLE BELOW)
      integer meventVal         ! number of event input data (SEE TABLE ABOVE)
      parameter (meventVal=5)
      integer neventVal         ! number of event input data (SEE TABLE ABOVE)
      real eventVal(max(1,meventVal)) ! event input data (SEE TABLE ABOVE)
      integer maxrep            ! maximum number of output reports
      parameter (maxrep=100)
      integer nrep              ! number of output reports
      real  repJD(maxrep)       ! time of output report (in julianDay)
      integer repId(maxrep)     ! output report identification (SEE TABLE ABOVE)
      real repVal(maxrep)       ! output report value (SEE TABLE ABOVE)
      character*250 rep250(maxrep) ! output report string (redundant)
      integer secdec            ! number of second decimals in output report string 
      character*250 crc250      ! error description
      integer irc               ! error return code (0=ok)
      integer lencrc
C
      real dtstartJD             ! start time (in julianDay)
      integer dsearchCode        ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
      real dtstopJD               ! report all events end time (in julianDay)
      integer deventId           ! requested event id (SEE TABLE BELOW)
      integer dneventVal         ! number of event input data (SEE TABLE ABOVE)
      real deventVal(max(1,meventVal)) ! event input data (SEE TABLE ABOVE)
C
      integer, external :: length
      character*1 :: ic         ! ichar of current character
      integer :: spos           ! start of segment
      integer :: ipos           ! equal position
      integer :: epos           ! end of segment
      logical :: bdone          ! are we done?
      integer :: charmin ! 
      integer :: charmax ! 
      parameter (charmin=ichar('a'), charmax=ichar('Z'))
      integer :: mode
      logical :: lstart,lstop,dstart,dstop,ldefault,lid
      integer :: cseq,oseq,idebug,lens,lenc
      integer :: yy1,mm1,dd1,hh1,mi1,yy2,mm2,dd2,hh2,mi2
      integer :: dyy1,dmm1,ddd1,dhh1,dmi1,dyy2,dmm2,ddd2,dhh2,dmi2
      integer :: yy,mm,dd,hh,mi
      integer :: values(8)
      integer :: ii, jj,istart,istop,nval
      real :: sec1,sec2,dsec1,dsec2,sec,cpuStart,cpuStop
      logical :: debug, bdeb
      data bdeb /.true./
      integer :: cost
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
C     eventStart: start time given as "YYYY-MM-DDTHH:MI:SSZ"
C     eventSearch: event search code; -1:previous, +1:next, 0: both, +2:until eventStop
C     eventStop: report all events until eventStop given as "YYYY-MM-DDTHH:MI:SSZ", only if eventSearch=+2.
C     eventId: requested event id (SEE TABLES BELOW)
C     eventVal<N>: input data (SEE TABLES BELOW) array, where <N> is the array index.
C     debug: report CPU-usage: 1 for active otherwise inactive.
C
      dsearchCode=1 ! default value always set
      deventId=210
      dneventVal=0
      dstart=.false. ! delay time calculation until we really need it
      dstop=.false.
      ldefault=.true.
      secdec=0
c
      oseq = -1                 ! current sequence number
      idebug=-1
      debug=.false.
      dneventVal=0
      nval=0
      do jj=1,meventVal
         deventVal(jj)=0.0D0
      end do
c
      call use_default()
C
      nline=0
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')"Content-Type: text/xml;"
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')" "
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')
     &     '<?xml version="1.0" encoding="utf-8"?>'
      lenline(nline)=len(trim(line250(nline)))
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)') '<astrodata xmlns:xsi='// ! 21
     &     '"http://www.w3.org/2001/XMLSchema-instance"'// ! 43
     &     ' xmlns="http://astro.met.no" xsi:schemaLocation='// ! 48
     &     '"http://astro.met.no/astro event.xsd">'// ! 38
     &     '<meta licenseurl="1"/>' ! 23
      lenline(nline)=len(trim(line250(nline)))
C    Returns End of Record error: 
C      nline=min(maxline,nline+1)
C      write(line250(nline),'(A)') '<query string='//
C     &     '"'// trim(s1000)//'"/>'
C      lenline(nline)=len(trim(line250(nline)))
C
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
            else if (s1000(ii:min(1000,ii+4)).eq.'event') then
               if (bdeb) write(*,*) 'Found "event"',ii,mode
               mode=1           ! search for seq or type
               ii=ii+5
               istart=ii
               istop=istart-1
            else if (s1000(ii:min(1000,ii+4)).eq.'debug') then
               if (bdeb) write(*,*) 'Found "debug"',ii,mode
               mode=2           ! search for debug flag
               ii=ii+5
            else ! strange character
               ii=ii+1
            end if
         else if (mode.eq.1) then ! "event*": search for seq or type
            if ( s1000(ii:ii).ge.'0'.and.
     &           s1000(ii:ii).le.'9') then ! number
               istop=ii
               ii=ii+1
            else if (s1000(ii:min(1000,ii+4)).eq.'Start') then
               if (bdeb) write(*,*) 'Found "Start"',ii,mode
               if (istart.le.istop) then
                  read(s1000(istart:istop),*,iostat=irc) cseq
                  if (irc.ne.0) then
                     cseq=oseq
                     irc=0
                  end if
                  if (cseq.ne.oseq.and.oseq.ne.-1) then ! we have a new sequence number
                     call execute_event()
                  end if
                  oseq=cseq
                  ldefault=.false.
               else
                  ldefault=.true.
               end if
               ii=ii+5
               mode=3           ! search for start time
            else if (s1000(ii:min(1000,ii+5)).eq.'Search') then
               if (bdeb) write(*,*) 'Found "Search"',ii,mode
               if (istart.le.istop) then
                  read(s1000(istart:istop),*,iostat=irc) cseq
                  if (irc.ne.0) then
                     cseq=oseq
                     irc=0
                  end if
                  if (cseq.ne.oseq.and.oseq.ne.-1) then ! we have a new sequence number
                     call execute_event()
                  end if
                  oseq=cseq
                  ldefault=.false.
               else
                  ldefault=.true.
               end if
               ii=ii+6
               mode=4           ! search for search type
            else if (s1000(ii:min(1000,ii+3)).eq.'Stop') then
               if (bdeb) write(*,*) 'Found "Stop"',ii,mode
               if (istart.le.istop) then
                  read(s1000(istart:istop),*,iostat=irc) cseq
                  if (irc.ne.0) then
                     cseq=oseq
                     irc=0
                  end if
                  if (cseq.ne.oseq.and.oseq.ne.-1) then ! we have a new sequence number
                     call execute_event()
                  end if
                  oseq=cseq
                  ldefault=.false.
               else
                  ldefault=.true.
               end if
               ii=ii+4
               mode=5           ! search for stop time
            else if (s1000(ii:min(1000,ii+1)).eq.'Id') then
               if (bdeb) write(*,*) 'Found "Id"',ii,mode
               if (istart.le.istop) then
                  read(s1000(istart:istop),*,iostat=irc) cseq
                  if (irc.ne.0) then
                     cseq=oseq
                     irc=0
                  end if
                  if(bdeb)write(*,*) 'Sequence:',cseq,oseq
                  if (cseq.ne.oseq.and.oseq.ne.-1) then ! we have a new sequence number
                     call execute_event()
                  end if
                  oseq=cseq
                  ldefault=.false.
               else
                  ldefault=.true.
               end if
               ii=ii+2
               mode=6           ! search for id number
            else if (s1000(ii:min(1000,ii+2)).eq.'Val') then
               if (bdeb) write(*,*) 'Found "val"',ii,mode
               if (istart.le.istop) then
                  read(s1000(istart:istop),*,iostat=irc) cseq
                  if (irc.ne.0) then
                     cseq=oseq
                     irc=0
                  end if
                  if (cseq.ne.oseq.and.oseq.ne.-1) then ! we have a new sequence number
                     call execute_event()
                  end if
                  oseq=cseq
                  ldefault=.false.
               else
                  ldefault=.true.
               end if
               ii=ii+3
               istart=ii
               istop=istart-1
               mode=7           ! search for value real
            else ! strange character
               ii=ii+1
            end if
         else if (mode.eq.2) then ! search for debug flag
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then
               if (bdeb) write(*,*) 'Found debug value: "'//
     &              s1000(istart:istop)//'"',ii,mode
               read(s1000(istart:istop),*,iostat=irc) idebug
               if (irc.ne.0) then
                  idebug=0
                  irc=0
               end if
               debug=(idebug.eq.1)
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.3) then ! search for start time
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
               if (bdeb) write(*,*) 'Found start time: "'//
     &              s1000(istart:istop)//'"',ii,mode
               read(s1000(istart:istop),*,iostat=irc) 
     &              yy1,mm1,dd1,hh1,mi1,sec1
               if (irc.ne.0) then
                  irc=0
                  lstart=.false.
               else
                  call date2jd(tstartJD,yy1,mm1,dd1,hh1,mi1,sec1)
                  if (ldefault) then
                     dstart=.true.
                     dtstartJD=tstartJD
                     dyy1=yy1
                     dmm1=mm1
                     ddd1=dd1
                     dhh1=hh1
                     dmi1=mi1
                     dsec1=sec1
                  end if
                  lstart=.true.
               end if
               ii=ii+1
               mode=0
            else
               if (s1000(ii:ii).eq.'-'.or.s1000(ii:ii).eq.':'.or.
     &              s1000(ii:ii).eq.'T'.or.s1000(ii:ii).eq.'Z') then
                  s1000(ii:ii)=' '
               end if
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.4) then ! search for search type
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
               if (bdeb) write(*,*) 'Found search code: "'//
     &              s1000(istart:istop)//'"',ii,mode
               read(s1000(istart:istop),*,iostat=irc) searchCode
               if (irc.ne.0) then
                  irc=0
                  searchCode=dsearchCode
               else
                  if (ldefault) dsearchCode=searchCode
               end if
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.5) then ! search for stop time
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
               if (bdeb) write(*,*) 'Found stop time: "'//
     &              s1000(istart:istop)//'"',ii,mode
               read(s1000(istart:istop),*,iostat=irc) 
     &              yy2,mm2,dd2,hh2,mi2,sec2
               if (irc.ne.0) then
                  irc=0
                  lstop=.false.
               else
                  call date2jd(tstopJD,yy2,mm2,dd2,hh2,mi2,sec2)
                  if (ldefault) then
                     dtstopJD=tstopJD
                     dyy2=yy2
                     dmm2=mm2
                     ddd2=dd2
                     dhh2=hh2
                     dmi2=mi2
                     dsec2=sec2
                     dstop=.true.
                  end if
                  lstop=.true.
               end if
               ii=ii+1
               mode=0
            else
               if (s1000(ii:ii).eq.'-'.or.s1000(ii:ii).eq.':'.or.
     &              s1000(ii:ii).eq.'T'.or.s1000(ii:ii).eq.'Z') then
                  s1000(ii:ii)=' '
               end if
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.6) then  ! search for id number
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then
               if (bdeb) write(*,*) 'Found event id: "'//
     &              s1000(istart:istop)//'"',ii,mode
               read(s1000(istart:istop),*,iostat=irc) eventId
               if (irc.ne.0) then
                  irc=0
                  eventId=deventId
                  lid=.false.
               else
                  if (ldefault) deventId=eventId
                  lid=.true.
               end if
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.7) then ! search for value index
            if (s1000(ii:ii).eq.'=') then
               if (istart.le.istop) then
                  if (bdeb) write(*,*) 'Found value index: "'//
     &                 s1000(istart:istop)//'"',ii,mode
                  read(s1000(istart:istop),*,iostat=irc) nval
                  if (irc.ne.0) then
                     irc=0
                     nval=1
                  end if
               else
                  nval=1
               end if
               istart=ii+1
               istop=ii
               mode=8           ! search for value
               nval=max(1,min(meventVal,nval))
               neventVal=max(neventVal,nval)
               if (ldefault) dneventVal=max(dneventVal,nval)
            else
               istop=ii
               ii=ii+1
            end if
         else if (mode.eq.8) then ! search for value
            if (s1000(ii:ii).eq.'=') then
               ii=ii+1
               istart=ii
            else if (s1000(ii:ii).eq.';'.or.s1000(ii:ii).eq.'&') then ! we are done
               if (bdeb) write(*,*) 'Found value: "'//
     &              s1000(istart:istop)//'"',ii,mode,nval
               read(s1000(istart:istop),*,iostat=irc) 
     &              eventVal(nval)
               if (irc.ne.0) then
                  irc=0
                  eventVal(nval)=deventVal(nval)
               else
                  if (ldefault) deventVal(nval)=eventVal(nval)
               end if
               ii=ii+1
               mode=0
            else
               istop=ii
               ii=ii+1
            end if
         end if
         bdone=(ii.gt.lens)
      end do
C
      if (lid) then
         if(bdeb)write(*,*)'Final event.'
         call execute_event()
      end if

      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')'</astrodata>'
      lenline(nline)=len(trim(line250(nline)))
C
      contains
C
      subroutine execute_event()
      implicit none
      character*250 :: s250
      character*50 :: val50
      integer :: jj,lens,leni,lenv,lenb,lenr
      if (.not.lstart) then
         call date_and_time(VALUES=values)
         yy1=values(1)
         mm1=values(2)
         dd1=values(3)
         hh1=values(5)
         mi1=values(6)
         sec1=real(values(7))
         call date2jd(tstartJD,yy1,mm1,dd1,hh1,mi1,sec1)
C     set default start time
         dstart=.true.
         dyy1=yy1
         dmm1=mm1
         dhh1=hh1
         dmi1=mi1
         dsec1=sec1
         dtstartJD=tstartJD
      end if
      if (searchCode.eq.2.and..not.lstop) then
         tstopJD=tstartJD+1.0D0
         call jd2date(tstopJD,yy2,mm2,dd2,hh2,mi2,sec2)
C     set default stop time
         dstop=.true.
         dyy2=yy2
         dmm2=mm2
         dhh2=hh2       
         dmi2=mi2
         dsec2=sec2
         dtstopJD=tstopJD
      else
         tstopJD=0.0D0
      end if
C
      if (debug) call cpu_time(cpuStart)
C
      nrep=0
      irc=0
      call astroEvent(
     &     tstartJD,            ! start time (in julianDay)
     &     searchCode,          ! search code; -1:previous, +1:next, 0: both, +2:until tstopJD
     &     tstopJD,             ! report all events end time (in julianDay)
     &     eventId,             ! requested event id (SEE TABLE BELOW)
     &     neventVal,           ! number of event input data (SEE TABLE BELOW)
     &     eventVal,            ! event input data (SEE TABLE BELOW)
     &     maxrep,              ! maximum number of output reports
     &     nrep,                ! number of output reports
     &     repJD,               ! time of output report (in julianDay)
     &     repId,               ! output report identification (SEE TABLE BELOW)
     &     repVal,              ! output report value (SEE TABLE BELOW)
     &     rep250,              ! output report string (redundant description)
     &     secdec,              ! number of second decimals used in output report string 
     &     crc250,              ! error description
     &     irc)                 ! error return code (0=ok)
C
      if (debug) call cpu_time(cpuStop)
      if (debug) then
         cost=nint((cpuStop-cpuStart)*1000.0D0)
      end if
C     
      if (irc.ne.0) then
         nrep=1
         call writeEventHdr(cost)
         lenc=length(crc250,250,10)
         write(s250,'(A,I0,A,I0,A)') 
     &        '<Report no="1" error="',irc,
     &        '" hint="'//crc250(1:lenc)//'"/>'
         call chop0(s250,250)
         lens=length(s250,250,50)
         nline=min(maxline,nline+1)
         line250(nline)='  '//s250(1:lens)
         lenline(nline)=len(trim(line250(nline)))
      else
         call writeEventHdr(cost)
         do jj=1,nrep
            call jd2date(repJD(jj),yy,mm,dd,hh,mi,sec)
            call chop0(rep250(jj),250)
            lenr=length(rep250(jj),250,10)
            if (nint(repVal(jj)).ne.-99999) then
               val50=getVal50(repVal(jj))
               call chop0(val50,50)
               lenv=length(val50,50,10)
               write(s250,'(A,I0,'//
     &              'A,I4.4,"-",I2.2,"-",I2.2,"T",'//
     &              'I2.2,":",I2.2,":",I2.2,"Z",'//
     &              'A,I0,2(A,A),A)')
     &              ' <Report no="',jj,
     &              '" time="',yy,mm,dd,hh,mi,int(sec),
     &              '" repId="',repId(jj),
     &              '" repVal="',val50(1:lenv),
     &              '" hint="',rep250(jj)(1:lenr),'"/>'
            else
               write(s250,'(A,I0,'//
     &              'A,I4.4,"-",I2.2,"-",I2.2,"T",'//
     &              'I2.2,":",I2.2,":",I2.2,"Z",'//
     &              'A,I0,A,A,A)')
     &              '<Report no="',jj,
     &              '" time="',yy,mm,dd,hh,mi,int(sec),
     &              '" repId="',repId(jj),
     &              '" hint="',rep250(jj)(1:lenr),'"/>'
            end if
            call chop0(s250,250)
            lens=length(s250,250,30)
            nline=min(maxline,nline+1)
            write(line250(nline),'(A)') '  '//s250(1:lens)
            lenline(nline)=len(trim(line250(nline)))
         end do
      end if
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')' </Event>'
      lenline(nline)=len(trim(line250(nline)))
C
      oseq=cseq ! update sequence number
C
      call use_default()
C
      return
      end subroutine execute_event
C
      subroutine use_default() 
      implicit none
      integer :: jj
C     Use default values
      if (dstart) then
         lstart=dstart
         tstartJD=dtstartJD
         yy1=dyy1
         mm1=dmm1
         dd1=ddd1
         hh1=dhh1
         mi1=dmi1
         sec1=dsec1
      else
         lstart=.false.
      end if
      searchCode=dsearchCode
      if (dstop) then
         lstop=dstop
         tstopJD=dtstopJD
         yy2=dyy2
         mm2=dmm2
         dd2=ddd2
         hh2=dhh2
         mi2=dmi2
         sec2=dsec2
      else
         lstop=.false.
      end if
      eventId=deventId
      neventVal=dneventVal
      do jj=1,meventVal
         eventVal(jj)=deventVal(jj)
      end do
      lid=.false.
      return
      end subroutine use_default
C
      character*50 function getVal50(val)
      implicit none
      character*50 :: buff
      integer :: j1,j2,kk
      real val
      if (abs(val-nint(val)).lt.1.0D-10) then
         write(getVal50,*)nint(val)
      else
         write(buff,'(F30.10)') val
         j1=0
         j2=30
         LOOP: do kk=30,1
            if (j1.eq.0.and.buff(kk:kk).eq.'.') then
               j1=kk
               exit LOOP
            else if (j1.eq.0.and.j2.eq.30.and.
     &              buff(kk:kk).ne.'0') then
               j2=kk
            end if
         end do LOOP
         if (j1.eq.j2.and.j1.ne.1) then
            getVal50=buff(1:j1-1)
         else
            getVal50=buff(1:j2)
         end if
      end if
      return
      end function getVal50

      subroutine writeEventHdr(cost)
      implicit none
      integer :: cost
      character*250 :: s250
      character*10 :: ind10
      character*50 :: val50
      integer :: jj,lens,leni,lenv,lenb,lenr
      if (searchCode.eq.2) then
         if (debug) then
            write(s250,'(A,I0,'//
     &           '2(A,I4.4,"-",I2.2,"-",I2.2,"T",'//
     &           'I2.2,":",I2.2,":",I2.2,"Z"),'//
     &           '3(A,I0),A,I0,A)')
     &           '   <Event Seq="',max(1,oseq),
     &           '" Start="',yy1,mm1,dd1,hh1,mi1,int(sec1),
     &           '" Stop="',yy2,mm2,dd2,hh2,mi2,int(sec2),
     &           '" Search="',searchCode,
     &           '" reports="',nrep,
     &           '" Id="',eventId,
     &           '" cost="',cost,'ms"'
         else
            write(s250,'(A,I0,'//
     &           '2(A,I4.4,"-",I2.2,"-",I2.2,"T",'//
     &           'I2.2,":",I2.2,":",I2.2,"Z"),'//
     &           '3(A,I0),A)')
     &           '   <Event Seq="',max(1,oseq),
     &           '" Start="',yy1,mm1,dd1,hh1,mi1,int(sec1),
     &           '" Stop="',yy2,mm2,dd2,hh2,mi2,int(sec2),
     &           '" Search="',searchCode,
     &           '" reports="',nrep,
     &           '" Id="',eventId,'"'
         end if
      else
         if (debug) then
            write(s250,'(A,I0,'//
     &           'A,I4.4,"-",I2.2,"-",I2.2,"T",'//
     &           'I2.2,":",I2.2,":",I2.2,"Z",'//
     &           '3(A,I0),A,I0,A)')
     &           '   <Event Seq="',max(1,oseq),
     &           '" Start="',yy1,mm1,dd1,hh1,mi1,int(sec1),
     &           '" Search="',searchCode,
     &           '" reports="',nrep,
     &           '" Id="',eventId,
     &           '" cost="',cost,'ms"'
         else
            write(s250,'(A,I0,'//
     &           'A,I4.4,"-",I2.2,"-",I2.2,"T",'//
     &           'I2.2,":",I2.2,":",I2.2,"Z",'//
     &           '3(A,I0),A)')
     &           '   <Event Seq="',max(1,oseq),
     &           '" Start="',yy1,mm1,dd1,hh1,mi1,int(sec1),
     &           '" Search="',searchCode,
     &           '" reports="',nrep,
     &           '" Id="',eventId,'"'
         end if
      end if
      call chop0(s250,250)
      lens=length(s250,250,30)
      do jj=1,neventVal
         write(ind10,'(I0)') jj
         call chop0(ind10,10)
         leni=length(ind10,10,5)
         val50=getVal50(eventVal(jj))
         call chop0(val50,50)
         lenv=length(val50,50,10)
         lenb=4+leni+2+lenv+1
         if (lenb+lens.le.250-1) then
            s250(lens+1:lens+lenb)=' Val'//ind10(1:leni)//
     &           '="'//val50(1:lenv)//'"'
            lens=lens+lenb
         end if
      end do
      s250(lens+1:lens+1)=">"
      lens=lens+1
      nline=min(maxline,nline+1)
      write(line250(nline),'(A)')' '//s250(1:lens)
      lenline(nline)=len(trim(line250(nline)))
      return
      end subroutine writeEventHdr

C     
      end subroutine event
