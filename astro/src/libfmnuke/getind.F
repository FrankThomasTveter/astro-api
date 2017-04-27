      BLOCK DATA BDGETIND
      IMPLICIT NONE
      LOGICAL ACTIVE,BDEB
      COMMON /CCGETIND/ACTIVE
      DATA ACTIVE/.false./
      END BLOCK DATA


      SUBROUTINE GETIND(IKODE,TYPE,SCNAME,AUX,CID250,CDR250,CLK250,
     &     IRC)
C
      IMPLICIT NONE
C
      INTEGER IKODE           ! -1=FILE WILL BE CREATED, 0=FILE MUST EXIST
      CHARACTER*12 TYPE,SCNAME,AUX
      CHARACTER*250 FILENM,CID250, CDR250, CLK250
      INTEGER IRC
C
      INTEGER NRHDR,LINE
      PARAMETER (NRHDR=250)
      CHARACTER*250 HDR250(NRHDR),DAT250,
     &     BUFF250,NUKEHEAD
      EXTERNAL NUKEHEAD
      INTEGER INTOUT
      LOGICAL ENDOFF
C
      LOGICAL LFLDAT(3),ACTIVE,BDEB,OK,FOUND,DONE
      INTEGER UNITI,FTUNIT,LENGTH,LENI,LEND,LENT,LENN,LENA,LENH,LENB
      EXTERNAL FTUNIT,LENGTH
      INTEGER MAXSAT,NRSAT,II,KODE
      PARAMETER (MAXSAT = 20)
      CHARACTER*250 SAT250(MAXSAT),SAT250B
      CHARACTER*12 SNAM12(MAXSAT),AUX12(MAXSAT),SNAM12B,AUX12B
      CHARACTER*16 MYNAME
C
      DATA MYNAME /'GETIND'/
      COMMON /CCGETIND/ACTIVE
C
      BDEB=.false.
C
      CALL CHOP0(SCNAME,12)
      LENN=LENGTH(SCNAME,12,3)
      CALL CHOP0(AUX,12)
      LENA=LENGTH(AUX,12,3)
C
      IF (LENN.EQ.0) THEN
         WRITE(*,*) MYNAME,'Invalid spacecraft name (length=0).'
         IRC=456
         RETURN
      END IF
C
      FILENM=CID250
      UNITI = FTUNIT(IRC)
      IF (IRC.NE.0) RETURN
      LENI = LENGTH(CID250,250,10)
      IF (IKODE.EQ.-1) THEN     ! CREATE FILE IF IT DOES NOT EXIST
         OPEN(UNIT=UNITI, FILE=CID250, STATUS='UNKNOWN',
     &        FORM='FORMATTED',IOSTAT=IRC)
      ELSE                      ! FILE MUST EXIST
         OPEN(UNIT=UNITI, FILE=CID250, STATUS='OLD',
     &        FORM='FORMATTED',IOSTAT=IRC)
      END IF
      IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'ERROR OPENING:',
     &        CID250(1:LENI)
         RETURN
      END IF
C
      LFLDAT(1)=.FALSE.
      LFLDAT(2)=.FALSE.
      LFLDAT(3)=.FALSE.
      NRSAT=0
C
      DO II=1,NRHDR
         HDR250(II) = ''
      ENDDO
C
      HDR250(1)='GETIND INDEX FILE V1.0 [0]'
      HDR250(2)='LIST OF SATELLITES AND INDEXED FILES [*]VFLR %'
      HDR250(3)='LIST OF SATELLITES, CATEGORIES AND'//
     &     ' INDEXED FILES [*]VFLR %'
C
      if (bdeb) then
         WRITE(*,*) MYNAME,'----------------------------------------'
         WRITE(*,*) MYNAME,'Reading index file:'//
     &        CID250(1:LENI)
      end if
C
C     READ DATA FROM INPUT FILE..............................
C
      KODE=-1
      CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,ENDOFF,IRC)
      IF (IRC.NE.0.AND.IKODE.EQ.-1) THEN
         WRITE(*,*) MYNAME,'Attempting to create index file.'
         IRC=0
      ELSE IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'Error return from NUKEM.'
         RETURN
      ELSE
C     
         LINE=INTOUT
C     
         KODE=0
         DO WHILE (.NOT.ENDOFF)
C     
C     READ NEXT DATA LINE
C     
            CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,
     &           ENDOFF,IRC)
            IF (IRC.NE.0) THEN
               WRITE(*,*) MYNAME,'Error return from NUKEM.'
               RETURN
            END IF
            LINE=INTOUT
C     
            IF (BDEB) WRITE(*,*) MYNAME,'Debug: Read header:',LINE
C     
C     CHECK WHAT LINE WE JUST READ
C     
            IF (LINE.EQ.1) THEN ! GETIND V1.0
               LFLDAT(1) =.TRUE.
            ELSEIF (LINE.EQ.2) THEN ! SATELLITE NAME
               IF (LFLDAT(3)) THEN
                  WRITE(*,*) MYNAME,'INVALID INDEX FILE SYNTAX:'//
     &                 CID250(1:LENI)
                  IRC=345
                  RETURN
               END IF
               NRSAT=NRSAT+1
               IF (NRSAT.GT.MAXSAT) THEN
                  WRITE(*,*) MYNAME,'TOO MANY SATELLITES'
                  IRC=88
                  RETURN
               END IF
C     
               buff250 = NUKEHEAD(DAT250,250)
               SNAM12(NRSAT)= buff250(1:12)
               SAT250(NRSAT)=DAT250
C     
               DO II=NRSAT,2,-1
                  IF (SNAM12(II).LT.SNAM12(II-1)) THEN
                     SNAM12B=SNAM12(II)
                     SAT250B=SAT250(II)
                     SNAM12(II)=SNAM12(II-1)
                     SAT250(II)=SAT250(II-1)
                     SNAM12(II-1)=SNAM12B
                     SAT250(II-1)=SAT250B
                  END IF
               END DO
C     
               LFLDAT(2) =.TRUE.
            ELSEIF (LINE.EQ.3) THEN ! SATELLITE NAME+AUX
               IF (LFLDAT(2)) THEN
                  WRITE(*,*) MYNAME,'INVALID INDEX FILE SYNTAX:'//
     &                 CID250(1:LENI)
                  IRC=345
                  RETURN
               END IF
               NRSAT=NRSAT+1
               IF (NRSAT.GT.MAXSAT) THEN
                  WRITE(*,*) MYNAME,'TOO MANY SATELLITES'
                  IRC=88
                  RETURN
               END IF
C     
               buff250 = NUKEHEAD(DAT250,250)
               SNAM12(NRSAT)= buff250(1:12)
               buff250 = NUKEHEAD(DAT250,250)
               AUX12(NRSAT)= buff250(1:12)
               SAT250(NRSAT)=DAT250
C     
               DO II=NRSAT,2,-1
                  IF (SNAM12(II).LT.SNAM12(II-1)) THEN
                     SNAM12B=SNAM12(II)
                     AUX12B=AUX12(II)
                     SAT250B=SAT250(II)
                     SNAM12(II)=SNAM12(II-1)
                     AUX12(II)=AUX12(II-1)
                     SAT250(II)=SAT250(II-1)
                     SNAM12(II-1)=SNAM12B
                     AUX12(II-1)=AUX12B
                     SAT250(II-1)=SAT250B
                  END IF
               END DO
C     
               LFLDAT(3) =.TRUE.
            ELSE IF (LINE.NE.0) THEN ! LINE.EQ.0 IMPLIES SOMETIMES EOF
               WRITE(*,*) MYNAME,
     &              'System error, line not implemented:',LINE
               IRC=999
               RETURN
            ENDIF
         ENDDO
C     
         KODE=1
         CALL NUKE(KODE,UNITI,FILENM,HDR250,INTOUT,DAT250,
     &        ENDOFF,IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from NUKEM.'
            RETURN
         END IF
C
      END IF
C     
C     MUST CHECK IF AUX IS EMPTY AND INDEX FILE EXPECTS NON-EMPTY
C     
      IF (LENA.EQ.0 .AND. LFLDAT(3)) THEN
         WRITE(*,*) MYNAME,'System error.'
         IRC=999
         RETURN
      END IF
C
      IF (LENA.NE.0 .AND. LFLDAT(2)) THEN
         WRITE(*,*) MYNAME,'System error.'
         IRC=998
         RETURN
      END IF
C
C     find location
C
      FOUND = .false.
      II=1
      DONE = (II.GT.NRSAT .OR. FOUND)
      DO WHILE (.NOT. DONE)
         IF (LENA.EQ.0) THEN
            IF (SNAM12(II).EQ.SCNAME) THEN
               CLK250 = SAT250(II)
               FOUND = .TRUE.
            ELSE
               II = II+1
            END IF
         ELSE
            IF (SNAM12(II).EQ.SCNAME .AND. AUX12(II).EQ.AUX) THEN
               CLK250 = SAT250(II)
               FOUND = .TRUE.
            ELSE
               II = II+1
            END IF
         END IF
         DONE = (II.GT.NRSAT .OR. FOUND)
      END DO
C
      IF (.NOT.FOUND .AND. IKODE.EQ.-1) THEN ! add sat if it does not exist
         FOUND = .TRUE.
         NRSAT=NRSAT+1
         IF (NRSAT.GT.MAXSAT) THEN
            WRITE(*,*) MYNAME,'TOO MANY SATELLITES'
            IRC=91
            RETURN
         END IF
         CALL CHOP0(CDR250,250)
         CALL CHOP0(TYPE,12)
         LEND=LENGTH(CDR250,250,10)
         LENT=LENGTH(TYPE,12,10)
         SNAM12(NRSAT)= SCNAME
         AUX12(NRSAT)= AUX
         IF (CDR250(LEND:LEND).EQ.'/') THEN
            IF (LENA.EQ.0) THEN
               SAT250(NRSAT)=CDR250(1:LEND)//
     &              SCNAME(1:LENN)//TYPE(1:LENT)
            ELSE
               SAT250(NRSAT)=CDR250(1:LEND)//
     &              SCNAME(1:LENN)//AUX(1:LENA)//TYPE(1:LENT)
            END IF
         ELSE
            IF (LENA.EQ.0) THEN
               SAT250(NRSAT)=CDR250(1:LEND)//'/'//
     &              SCNAME(1:LENN)//TYPE(1:LENT)
            ELSE
               SAT250(NRSAT)=CDR250(1:LEND)//'/'//
     &              SCNAME(1:LENN)//AUX(1:LENA)//TYPE(1:LENT)
            END IF
         END IF
         CALL CHOP0(SAT250(NRSAT),250)
         CLK250 = SAT250(NRSAT)
C
         DO II=NRSAT,2,-1
            IF (SNAM12(II).LT.SNAM12(II-1)) THEN
               IF (LENA.EQ.0) THEN
                  SNAM12B=SNAM12(II)
                  SAT250B=SAT250(II)
                  SNAM12(II)=SNAM12(II-1)
                  SAT250(II)=SAT250(II-1)
                  SNAM12(II-1)=SNAM12B
                  SAT250(II-1)=SAT250B
               ELSE
                  SNAM12B=SNAM12(II)
                  AUX12B=AUX12(II)
                  SAT250B=SAT250(II)
                  SNAM12(II)=SNAM12(II-1)
                  AUX12(II)=AUX12(II-1)
                  SAT250(II)=SAT250(II-1)
                  SNAM12(II-1)=SNAM12B
                  AUX12(II-1)=AUX12B
                  SAT250(II-1)=SAT250B
               END IF
            END IF
         END DO
      ELSE IF (.NOT. FOUND) THEN
         WRITE(*,*) MYNAME,'No data file found for satellite:'//
     &        SCNAME(1:LENN), '('//AUX(1:LENA)//')'
         IRC=92
         RETURN
      END IF
C
      CLOSE(UNITI)
C
C     WRITE TO FILE
C
      IF (IKODE.EQ.-1) THEN
         OPEN(UNIT=UNITI, FILE=CID250, STATUS='UNKNOWN',
     &        FORM='FORMATTED',IOSTAT=IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'ERROR OPENING:',
     &           CID250(1:LENI)
            RETURN
         END IF
C
         buff250='#!NUKE'
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         buff250='#'
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         buff250=HDR250(1)
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         buff250='#'
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         IF (NRSAT.NE.0) THEN
C
            IF (LENA.EQ.0) THEN
               buff250=HDR250(2)
            ELSE
               buff250=HDR250(3)
            END IF
            call chop0(buff250,250)
            lenB=length(buff250,250,3)
            WRITE(UNITI,*) buff250(1:lenb)
            DO II=1,NRSAT
               lenn=length(SNAM12(II),12,3)
               lena=length(AUX12(II),12,3)
               lend=length(SAT250(II),250,10)
               IF (LENA.EQ.0) THEN
                  buff250=SNAM12(II)(1:LENN)//' '//
     &                 SAT250(II)(1:MIN(LEND,250-1-LENN))
               ELSE
                  buff250=SNAM12(II)(1:LENN)//' '//
     &                 AUX12(II)(1:LENA)//' '//
     &                 SAT250(II)(1:MIN(LEND,250-2-LENN-LENA))
               END IF
               call chop0(buff250,250)
               lenB=length(buff250,250,3)
               WRITE(UNITI,*) buff250(1:lenb)
            END DO
         ELSE
            buff250='# NO SATELLITE DEFINED.'
            call chop0(buff250,250)
            lenB=length(buff250,250,3)
            WRITE(UNITI,*) buff250(1:lenb)
         END IF
C
         buff250='#'
         call chop0(buff250,250)
         lenB=length(buff250,250,3)
         WRITE(UNITI,*) buff250(1:lenb)
C
         CLOSE(UNITI)
C
         IF (.NOT.FOUND) THEN
            WRITE(*,*) MYNAME,'Undefined satellite:',SCNAME
            IRC=93
            RETURN
         END IF
      END IF
C
      if (bdeb) then
         WRITE(*,*) MYNAME,'----------------------------------------'
      end if
C
      RETURN
      END
