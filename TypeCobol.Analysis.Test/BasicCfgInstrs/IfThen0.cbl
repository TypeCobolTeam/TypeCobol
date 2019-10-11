       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFTHEN.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC P(2) VALUE 10.
       PROCEDURE DIVISION.
           IF A = 10 THEN
             DISPLAY "A = 10"
             DISPLAY "RIGHT ?"
           END-IF.
       END PROGRAM IFTHEN.
      