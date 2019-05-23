       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFTHEN.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(4).
       01 WS-B PIC 9(4).
       01 WS-C PIC 9(4).
       PROCEDURE DIVISION.
           MOVE 100 TO WS-A.
           MOVE 200 TO WS-B.
           MOVE 300 TO WS-C.
           IF WS-A > ZERO AND WS-B > ZERO THEN
             NEXT SENTENCE
           ELSE
             DISPLAY 'One of the input amount are positive '
             DISPLAY 'WS-A value ', WS-A
             DISPLAY 'WS-B value ', WS-B
           END-IF
           ADD WS-A TO WS-C.
           DISPLAY 'WS-C value ', WS-C.
       END PROGRAM IFTHEN.
      