      PROGRAM ASTRO
C     
C     ASTRO PROGRAM FOR DATA TEST READ
C
      IMPLICIT NONE
C
      INTEGER UNITI,IRC
      character*250 crc250
      CHARACTER*10 MYNAME
      DATA MYNAME /'ASTRO'/
      LOGICAL BDEB,ACTIVE
      DATA BDEB /.FALSE./
      DATA ACTIVE /.FALSE./
      integer length,lencrc
      external length
C     
      IRC=0
      crc250=''
      UNITI=5
C
C     Debug System.
C
      IF(.NOT.ACTIVE)CALL DEBUG(MYNAME,BDEB,ACTIVE)
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Program starts.',IRC
      CALL BLOBB
      CALL MASTRO(UNITI,crc250,IRC)
      IF (IRC.NE.0) THEN
         lencrc=length(crc250,250,100)
         write(*,*) ">"//crc250(1:lencrc)//"<"
         WRITE(*,*) MYNAME,'Error return from MASTRO.',IRC
      ENDIF
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Program ends.',IRC
C
      IF (IRC.EQ.0) THEN
         WRITE(*,*) MYNAME,'-------NORMAL END OF PROGRAM-------'
      ELSE
         WRITE(*,*) MYNAME,'--------ERROR WHILE RUNNING--------',IRC
      ENDIF
      IRC=MIN(1,IRC)
C
      CALL exit(IRC)
C
      END
