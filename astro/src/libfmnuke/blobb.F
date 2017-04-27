      SUBROUTINE BLOBB
C
      IMPLICIT NONE
C
      CHARACTER*72 STR(29)
      INTEGER JJ
C
      STR(1)='                                   '//
     &     '                          ..         '
      STR(2)='                                  ,,'//
     &     ',                         MM .M     '
      STR(3)='                              ,!MMMM'//
     &     'MMM!,                     MM MM  ,. '
      STR(4)='      ., .M                .MMMMMMMM'//
     &     'MMMMMMMM.,          `MM.  MM MM .M` '
      STR(5)='    . M: M;  M          .MMMMMMMMMMM'//
     &     'MMMMMMMMMMM,          `MM,:M M`!M`  '
      STR(6)='   ;M MM M: .M        .MMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMM,         `MM`...`M    '
      STR(7)='    M;MM;M :MM      .MMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMM.       .MMMMMMMM    '
      STR(8)='    `M;M`M MM      MMMMMM  MMMMMMMMM'//
     &     'MMMMMMMM  MMMMMM.    ,,M.M.`MMM`    '
      STR(9)='     MM`MMMM      MMMMMM @@ MMMMMMMM'//
     &     'MMMMMMM @@ MMMMMMM.`M``MMMM;MM`     '
      STR(10)='    MM., ,MM     MMMMMMMM  MMMMMMMMM'//
     &     'MMMMMMMM  MMMMMMMMM      `.MMM      '
      STR(11)='    `MM;MMMMMMMM.MMMMMMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMMMMMM.      `MMM      '
      STR(12)='     ``.`MMM`  .MMMMMMMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMMMMMMM       MMMM     '
      STR(13)='      MMC      MMMMMMMMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMMMMMMM.      `MMMM    '
      STR(14)='     .MM      :MMMMMMMMMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMMMMMMM``MMM       MMMMM   '
      STR(15)='     MMM      :M  `MMMMMMMMMMMMM.MMM'//
     &     'MM.MMMMMMMMMM`.MM  MM:M.    `MMMMM  '
      STR(16)='    .MMM   ...:M: :M.`MMMMMMMMMMMMMM'//
     &     'MMMMMMMMMMM`.M``   MM:MMMMMMMMMMMM` '
      STR(17)='   AMMM..MMMMM:M.    :M.`MMMMMMMMMMM'//
     &     'MMMMMMMMM`.MM`     MM````````````   '
      STR(18)='   MMMMMMMMMMM:MM     `M`.M`MMMMMMMM'//
     &     'MMMMMM`.MC`M`     .MM               '
      STR(19)='    ``````````:MM.       `MM!M.`M-M-'//
     &     'M-M`M.`MM`        MMM               '
      STR(20)='               MMM.            `MMMM'//
     &     '!MMMM`            .MM               '
      STR(21)='                MMM.             ```'//
     &     '   ``            .MM`               '
      STR(22)='                 MMM.               '//
     &     '                MMM`                '
      STR(23)='                  MMMM            ,.'//
     &     'J.JJJJ.       .MMM`                 '
      STR(24)='                   MMMM.       `JJJJ'//
     &     'JJJ`JJJM   CMMMMM                   '
      STR(25)='                     MMMMM.    `JJJJ'//
     &     'JJJJ`JJJ .MMMMM`                    '
      STR(26)='                       MMMMMMMM.`  `'//
     &     'JJJJJ`JJMMMMM`                      '
      STR(27)='                         `MMMMMMMMM`'//
     &     'JJJJJ JJJJJ`                        '
      STR(28)='                            ``MMMMMM'//
     &     'JJJJJJJJJJ`                         '
      STR(29)='                                    '//
     &     '`JJJJJJJJ`                          '
C
C     WRITE "BLOBB" TO SCREEN
C
C      WRITE(*,*)
C      DO JJ=1,29
C         WRITE(*,*) STR(JJ)
C      ENDDO
C      WRITE(*,*)
C
      RETURN
      END
