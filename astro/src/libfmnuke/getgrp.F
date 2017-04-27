      subroutine GETGRP(BUFF250,chdr,
     &     MAXGRP,MINGRP,NRGRP,GROUPS,
     &     IRC)
      implicit none
C
      character*250 buff250
      integer chdr
      integer maxgrp
      integer mingrp
      integer nrgrp
      integer groups(6,maxgrp) ! 1=parent, 2=type, 3=id/chdr,4=lvl
      integer irc
C
      integer cnrgrp,fii,ii,fcii,cii,jj,copt,chii,tii,nchii
      logical bdone,bbdone,bcom,bnum,bnew
      integer cgroups(6,maxgrp) ! 1=parent, 2=next sibling, 3=first child,  4=type, 5=id/chdr, 6=lvl
      integer lenh,length,num,tnum
      external length
      character*1 opt
C      
      character*16 myname
      data myname /'GETGRP'/
C
C     CHECK FOR GROUP FLAGS
C     
      LENH=LENGTH(BUFF250,250,10)
      JJ=1
      BNEW=.TRUE.
      BBDONE=.false.            ! always enter loop
      DO WHILE (.NOT.BBDONE)
C
C     make next group chain for this header
C
         copt=2                 ! default type is "%"
         cnrgrp=0
         BDONE=(jj.gt.lenh)
         DO WHILE (.NOT.BDONE)
            OPT=BUFF250(JJ:JJ)
            BCOM=.FALSE.
            BNUM=.FALSE.
            IF (OPT.EQ.'&') THEN ! mandatory
               BCOM=.TRUE.
               COPT=1
            ELSE IF (OPT.EQ.'%') THEN ! 1 or 0
               BCOM=.TRUE.
               COPT=2
            ELSE IF (OPT.EQ.'$') THEN ! 1 or 0 or absent
               BCOM=.TRUE.
               COPT=3
            ELSE IF (OPT.EQ.'0') THEN 
               BNUM=.TRUE.
               NUM=0
            ELSE IF (OPT.EQ.'1') THEN 
               BNUM=.TRUE.
               NUM=1
            ELSE IF (OPT.EQ.'2') THEN 
               BNUM=.TRUE.
               NUM=2
            ELSE IF (OPT.EQ.'3') THEN 
               BNUM=.TRUE.
               NUM=3
            ELSE IF (OPT.EQ.'4') THEN 
               BNUM=.TRUE.
               NUM=4
            ELSE IF (OPT.EQ.'5') THEN 
               BNUM=.TRUE.
               NUM=5
            ELSE IF (OPT.EQ.'6') THEN 
               BNUM=.TRUE.
               NUM=6
            ELSE IF (OPT.EQ.'7') THEN 
               BNUM=.TRUE.
               NUM=7
            ELSE IF (OPT.EQ.'8') THEN 
               BNUM=.TRUE.
               NUM=8
            ELSE IF (OPT.EQ.'9') THEN 
               BNUM=.TRUE.
               NUM=9
            ELSE IF (OPT.EQ.' ') THEN 
               BNEW=.TRUE.
            ELSE
               WRITE(*,*) MYNAME,
     &              'Error: unknown header option, ',
     &              OPT,' in '//buff250(1:lenh)
               IRC=999
               RETURN
            END IF
            IF (BCOM) THEN      ! new group
               TNUM=0
               cNRGRP=cNRGRP+1
               IF (cNRGRP.GT.MAXGRP) THEN
                  WRITE(*,*) MYNAME,
     &                 'Error: too many groups in '//buff250(1:lenh)
                  irc=932
                  return
               END IF
               cgroups(1,cnrgrp)=cnrgrp-1 ! parent
               cgroups(2,cnrgrp)=0 ! next sibling
               cgroups(3,cnrgrp)= 0 ! first child
               cgroups(4,cnrgrp)=copt ! type
               cgroups(5,cnrgrp)=0 ! id/hdr nr
               cgroups(6,cnrgrp)=cnrgrp ! level
               if (cgroups(1,cnrgrp).gt.0) then ! update parent
                  cgroups(3,cgroups(1,cnrgrp))=cnrgrp ! first child
               end if
               bnew=.false.
            ELSE IF (BNUM) THEN !
               IF (BNEW) THEN
                  WRITE(*,*) MYNAME,
     &                 'Error: invalid group definitions in"'//
     &                 buff250(1:lenh),'" header no:',chdr
                  irc=974
                  return
               END IF
               TNUM=TNUM*10+NUM
               cgroups(5,cnrgrp)=tnum ! id/hdr nr
            END IF
            jj=jj+1
            BDONE=(BNEW.OR.JJ.GT.LENH)
         END DO
C     
C     define default groups
C
         if (cnrgrp.eq.0) then
            cnrgrp=1
            copt=2              ! default is the "optional group"
            cgroups(1,cnrgrp)=cnrgrp-1 ! parent
            cgroups(2,cnrgrp)=0 ! no sibling
            cgroups(3,cnrgrp)=0 ! child
            cgroups(4,cnrgrp)=copt ! type is "optional group"
            cgroups(5,cnrgrp)=0 ! no id yet
            cgroups(6,cnrgrp)=cnrgrp ! top level (=0)
         end if

         do ii=1,cnrgrp
            if (cgroups(5,II).eq.0) then
               mingrp=mingrp-1
               cgroups(5,II)=mingrp
            end if
         end do
C
C     add final header group (which must be unique)
C
         cnrgrp=cnrgrp+1
         if (cnrgrp.gt.maxgrp) then
            WRITE(*,*) MYNAME,
     &           'Error: too many groups ',cnrgrp
            irc=984
            return
         end if
         cgroups(1,cnrgrp)=cnrgrp-1    ! parent
         cgroups(2,cnrgrp)=0    ! no siblings
         cgroups(3,cnrgrp)=0    ! no children
         cgroups(4,cnrgrp)=-1     ! type
         cgroups(5,cnrgrp)=-chdr  ! id
         cgroups(6,cnrgrp)=cnrgrp ! level
         if (cgroups(1,cnrgrp).gt.0) then ! update parent
            cgroups(3,cgroups(1,cnrgrp))=cnrgrp ! first child
         end if
C
C     find group from head down in archive
C
         fcii=0
         fii=0
         ii=1
         cii=1
         bdone=(ii.gt.nrgrp)
         do while (.not.bdone)
            if (groups(1,ii).eq.cgroups(1,cii).and. ! same parent
     &           groups(4,ii).eq.cgroups(4,cii).and. ! same type
     &           groups(5,ii).eq.cgroups(5,cii).and. ! same id
     &           groups(6,ii).eq.cgroups(6,cii)) then ! same lvl
               fii=ii
               fcii=cii
               if (cii.eq.cnrgrp) then ! this is last group
                  write(*,*) myname,'This should never happen...'
                  irc=999
                  return
               end if
               cii=cii+1
               cgroups(1,cii)=ii ! redefine the parent of first child
C     
C     a child may never be before the parent in archive,
C     we do therefore not need to reset ii...
C
            else
               ii=ii+1
               bdone=(ii.gt.nrgrp)
            end if
         end do
C
C     add last part of chain not found in archive
C
         tii=nrgrp+1            ! the top of the added chain
         do cii=fcii+1,cnrgrp
            nrgrp=nrgrp+1
            if (nrgrp.gt.maxgrp) then
               WRITE(*,*) MYNAME,
     &              'Error: too many groups ',nrgrp
               irc=984
               return
            end if
            groups(1,nrgrp)=fii !parent
            groups(2,nrgrp)=0 ! sibling defined later for top level
            if (cgroups(3,cii).ne.0) then
               groups(3,nrgrp)=nrgrp+1 ! child is next object
            else
               groups(3,nrgrp)=0 ! child
            end if
            groups(4,nrgrp)=cgroups(4,cii) ! type
            groups(5,nrgrp)=cgroups(5,cii) ! id
            groups(6,nrgrp)=cgroups(6,cii) ! lvl
            fii=nrgrp
         end do
C
C     add to sibling-chain or update parents first child-index
C
         chii=0
         if (groups(1,tii).gt.0) then ! there exists a parent
            nchii=groups(3,groups(1,tii)) !parent's first child
         else
            nchii=groups(2,1) ! sibling of first top-level object
         end if
         bdone=(nchii.eq.0)     ! parent has no other children?
         do while (.not.bdone)
            chii=nchii
            nchii=groups(2,chii)
            bdone=(nchii.eq.0)  ! parent has no other children?
         end do
         if (chii.eq.0) then    ! parents first child is this one
            if (groups(1,tii).eq.0) then
               if (tii.gt.1) 
     &              groups(2,1)=tii ! first sibling of top object
            else
               groups(3,groups(1,tii))=tii ! first child of parent
            end if
         else                
            groups(2,chii)=tii  ! last child in a row
         end if
C     
      BBDONE=(JJ.GT.LENH)
      END DO
C     
      return
      end 






