       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TheTowersOfHanoi.
       AUTHOR.        Amit Singh <http://hanoi.kernelthread.com>.
      
       ENVIRONMENT DIVISION.
      
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. ALMOST-PORTABLE.
       OBJECT-COMPUTER. ALMOST-PORTABLE.
      
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
      
       01  STACK-SPACE.
           02  ESP            PIC S9(3) COMP.
           02  STACK-FRAME    OCCURS 1024.
               03  S-N        PIC 9(1).
               03  S-FROM     PIC X(1).
               03  S-USING    PIC X(1).
               03  S-TO       PIC X(1).
               03  S-PROC     PIC 9(1).
      
       01  CURRENT-FRAME.
           02  CN        PIC 9(1)    VALUE 3.
           02  CFROM     PIC X(1)    VALUE "1".
           02  CUSING    PIC X(1)    VALUE "2".
           02  CTO       PIC X(1)    VALUE "3".
           02  CPROC     PIC 9(1)    VALUE 0.
      
       01  TMP-FRAME.
           02  TN        PIC 9(1)    VALUE 3.
           02  TFROM     PIC X(1)    VALUE "1".
           02  TUSING    PIC X(1)    VALUE "2".
           02  TTO       PIC X(1)    VALUE "3".
           02  TPROC     PIC 9(1)    VALUE 0.
      
       PROCEDURE DIVISION.
       BEGIN-PROGRAM.
           PERFORM GET-DISKS
           MOVE 1 TO ESP
           MOVE CURRENT-FRAME TO STACK-FRAME (ESP)
           PERFORM DO-HANOI
                 UNTIL ESP = ZERO
           .
           STOP RUN
           .
      
       DO-HANOI.
           MOVE STACK-FRAME (ESP) TO CURRENT-FRAME
           SUBTRACT 1 FROM ESP
           IF CPROC = 0
               IF CN = 1
                   PERFORM MOVE-DISK
               ELSE
                   MOVE CN TO TN
                   MOVE CFROM TO TFROM
                   MOVE CUSING TO TUSING
                   MOVE CTO TO TTO
                   MOVE 1 TO TPROC
                   ADD 1 TO ESP
                   MOVE TMP-FRAME TO STACK-FRAME (ESP)
                   MOVE CN TO TN
                   SUBTRACT 1 FROM TN
                   MOVE CFROM TO TFROM
                   MOVE CTO TO TUSING
                   MOVE CUSING TO TTO
                   MOVE 0 TO TPROC
                   ADD 1 TO ESP
                   MOVE TMP-FRAME TO STACK-FRAME (ESP)
               END-IF
           ELSE
               PERFORM MOVE-DISK
               MOVE 0 TO TPROC
               MOVE CTO TO TTO
               MOVE CFROM TO TUSING
               MOVE CUSING TO TFROM
               MOVE CN TO TN
               SUBTRACT 1 FROM TN
               ADD 1 TO ESP
               MOVE TMP-FRAME TO STACK-FRAME (ESP)
           END-IF
           .
      
       MOVE-DISK.
               DISPLAY CFROM
               "--> "
               CTO
           .
      
       GET-DISKS.
           DISPLAY "How many disks to solve for? " NO ADVANCING
           ACCEPT CN.
           IF CN < 1 OR CN > 9
               DISPLAY "Invalid number of disks (1 <= N <= 9)."
               EXIT PROGRAM
           END-IF
           .
       END PROGRAM TheTowersOfHanoi.
      