      SUBROUTINE GETCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,IRC)
C
      IMPLICIT NONE
C
      INTEGER MAXCOM, NRCOM
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      INTEGER CMLEN(MAXCOM),IRC
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'GETCOM'/
      INTEGER INRCOM,II,LENGTH
      EXTERNAL LENGTH
      DATA INRCOM /13/
C
C     DEFINE COMMANDS
C
      NRCOM=INRCOM
      IF (MAXCOM.LT.INRCOM) THEN
         WRITE(*,*) MYNAME,'System error.'
         IRC=999
         RETURN
      END IF
C     
      COM20(1)='if'
      HLP20(1)='(<expr>)'
      COM20(2)='elseif'
      HLP20(2)='(<expr>)'
      COM20(3)='else'
      CALL RESET(HLP20(3),20)
      COM20(4)='endif'
      CALL RESET(HLP20(4),20)
      COM20(5)='!'
      HLP20(5)='<UNIX command>'
      COM20(6)='include'
      HLP20(6)='(<file>)'
      COM20(7)='stop'
      CALL RESET(HLP20(7),20)
      COM20(8)='exit'
      HLP20(8)='<exit code>'
      COM20(9)='echo'
      HLP20(9)='<text>'
      COM20(10)='set'
      HLP20(10)='<var>=<expr[$(var)]>'
      COM20(11)='import'
      HLP20(11)='<var1> <var2> ...'
      COM20(12)='xx'
      CALL RESET(HLP20(12),20)
      COM20(13)='x'
      CALL RESET(HLP20(13),20)
C
      DO II=1,NRCOM
         CALL CHOP0(COM20(II),20)
         CALL CHOP0(HLP20(II),20)
C     FIND LENGTH OF COMMAND
         CMLEN(II)=LENGTH(COM20(II),20,3)
C     write(*,*) COM20(II)(1:CMLEN(II))
      ENDDO
C
C     
      RETURN
      END
