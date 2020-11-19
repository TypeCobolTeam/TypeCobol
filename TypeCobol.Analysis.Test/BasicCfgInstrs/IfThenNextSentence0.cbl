       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFTHEN.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(4) VALUE 10.
       01 B PIC 9(4).
       PROCEDURE DIVISION.
           IF A = 10 THEN
             NEXT SENTENCE
           END-IF
           DISPLAY "AFTER".
           DISPLAY "CONTINUE".
       END PROGRAM IFTHEN.
      