      BLOCK DATA BDSUBSTR
      IMPLICIT NONE
      INTEGER LENS1,LENS2
      COMMON /CCSUBSTR/LENS1,LENS2
      DATA LENS1/1/,LENS2/1/
      END BLOCK DATA


      SUBROUTINE SUBSTR(STRING1,N1,STRING2,N2,POS1,DELL)
C     ***********************************************************************
C     +                                                                     *
C     SUBSTITUTES PART OF STRING1 WITH STRING2                              *
C     +                                                                     *
CIO   STRING1 (C*N1) = STRING WHICH WILL HAVE A SUBSTRING REPLACED          *
CI    N1 (I*4) = LENGTH OF STRING1                                          *
CI    STRING2 (C*N2) = STRING WHICH WILL BE COPIED OVER TO STRING1          *
CI    N2 (I*4) = LENGTH OF STRING2                                          *
CI    POS1 (I*4) = START POSITION IN STRING1                                *
CI    DELL (I*4) = NUMBER OF CHARACTERS TO DELETE IN STRING1                *
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE   |                 REASON                  | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 18/07/95 | NEW                                     | *
C     |              |          |                                         | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER N1,N2,POS1,DELL
      CHARACTER*(*) STRING1,STRING2
C
      INTEGER LENS1,LENS2,LENGTH,MAXX,TRG,DELTA,POSS,LL
      EXTERNAL LENGTH

C     common /ccSUBSTR/LENS1,LENS2
C
C     REMOVE VALUABLE DATA FROM TARGET AREA
C
C     CALL CHOP0(STRING1,N1)
      LENS1=LENGTH(STRING1,N1,LENS1)
C
      CALL CHOP0(STRING2,N2)
      LENS2=LENGTH(STRING2,N2,LENS2)
C
      IF((POS1+LENS2-1).GT.N1)LENS2=N1+1-POS1
      IF (POS1+DELL-1.GT.LENS1)DELL=LENS1-POS1+1
C
      DELTA=(LENS2-DELL)
C
      MAXX=N1
      MAXX=MIN(MAXX,LENS1+DELTA)
      MAXX=MAXX-DELTA
C
C     MAXX IS LAST DATA (STILL IN ARRAY)
C
      TRG=POS1+DELL
      IF (DELTA.GT.0) THEN
         DO LL=MAXX,TRG,-1
            STRING1(LL+DELTA:LL+DELTA)=STRING1(LL:LL)
         ENDDO
      ELSE
         DO LL=TRG,MAXX
            STRING1(LL+DELTA:LL+DELTA)=STRING1(LL:LL)
         ENDDO
C
C     REMOVE OLD DATA IN EMPTY SLOT (AFTER LAST DATA)
C
         MAXX=MAXX+DELTA+1
         DO LL=MAXX,LENS1
            STRING1(LL:LL)=' '
         ENDDO
      ENDIF
C
      TRG=POS1+LENS2-1
      IF (TRG.GT.N1) TRG=N1
      DO LL=POS1,TRG
         POSS=LL-POS1+1
         STRING1(LL:LL)=STRING2(POSS:POSS)
      ENDDO
C
      RETURN
      END
