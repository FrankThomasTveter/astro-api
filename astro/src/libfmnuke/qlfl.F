      logical function QLFL(MAXGRP,NRGRP,
     &     BHDRGRPA,BHDRGRPB)
C
      implicit none
C
      integer maxgrp, nrgrp
      integer BHDRGRPA(2,maxgrp),BHDRGRPB(2,maxgrp)
C
      logical later,bdone
      integer ii
C
      II=1
      later=.TRUE.
      bdone=(ii.gt.nrgrp)
      DO WHILE (.not. bdone)
         if (bhdrgrpa(1,ii).EQ.bhdrgrpb(1,ii) .and.
     &        bhdrgrpa(2,ii).EQ.bhdrgrpb(2,ii)) then
            ii=ii+1
         else if (bhdrgrpa(2,ii).lt.bhdrgrpb(2,ii) .OR.
     &           (bhdrgrpa(1,ii).lt.bhdrgrpb(1,ii) .and.
     &        bhdrgrpa(2,ii).EQ.bhdrgrpb(2,ii))) then
            later=.true.
            bdone=.true.
         else
            later=.false.
            bdone=.true.
         end if
         if (.not.bdone) bdone=(ii.gt.nrgrp)
      END DO
C
      qlfl=later
      return
      end
