      subroutine heapsort1(mm,key1,newnn,nn,ind,eps,uniq)
C
C     ! Generate sorted index for key1 
C
      implicit none
      
      integer mm                ! Number of elements
      real    key1(mm)          ! First key
      integer newnn
      integer nn                ! Number of elements
      integer ind(nn)           ! Resulting sorted index
      real    eps               ! Delta key1
      logical uniq               ! Ignore duplicate records
C
      integer ii,dmp
      
      if (nn.eq.0) then
         newnn=0
         return
      end if
C
      do ii = nn/2, 1, -1
         call pushdown(ii, nn)
      end do
      do ii = nn, 2, -1
         call swap(ind(1), ind(ii))
         call pushdown(1, ii-1)
      end do
C
      if (uniq) then
         dmp=0
         newnn=1
         do ii=2,nn
            if (cmp(ii-1,ii) /= 0) then
C Keep ind(ii)
               newnn = newnn+1
               ind(newnn) = ind(ii)
            else
               dmp=dmp+1
            end if
         end do
C     write(*,*) "HEAPSORT dumped elements:",dmp
      else
         newnn=nn
      end if
C
      contains
C
C
C
      subroutine pushdown(first, last)
C
      integer first
      integer last
C
      integer r
C
      r = first
C
      MAINLOOP: do while (r <= last/2)
         if (last == 2*r) then
            if (cmp(r, 2*r) > 0) then
c  if (key1(ind(r)) > key1(ind(2*r))) then
               call swap(ind(r), ind(2*r))
            end if
            exit MAINLOOP
         else
            if (cmp(r,2*r) > 0 .and. cmp(2*r,2*r+1) <= 0) then
c            if (key1(ind(r)) > key1(ind(2*r))
c     &           .and. key1(ind(2*r)) <= key1(ind(2*r+1))) then
               call swap(ind(r), ind(2*r))
               r = 2*r
            else if (cmp(r,2*r+1)>0 .and. cmp(2*r+1,2*r)<0) then
c            else if (key1(ind(r)) > key1(ind(2*r+1))
c     &              .and. key1(ind(2*r+1)) < key1(ind(2*r))) then
               call swap(ind(r), ind(2*r+1))
               r = 2*r+1
            else
               exit MAINLOOP
            end if
         end if
      end do MAINLOOP
C
      end subroutine pushdown
C
C
      integer function cmp(a, b)
      
      integer a
      integer b
C
      if (abs(key1(ind(a))-key1(ind(b))) < eps) then
         cmp = 0
      else if (key1(ind(a)) < key1(ind(b))) then
         cmp = 1
      else
         cmp = -1
      end if
C
      end function cmp
C
C
      subroutine swap(k1, k2)
C
      implicit none
C
      integer k1
      integer k2
C
      integer tmp
C
      tmp = k1
      k1 = k2
      k2 = tmp
C
      end subroutine swap
C
      end subroutine heapsort1
