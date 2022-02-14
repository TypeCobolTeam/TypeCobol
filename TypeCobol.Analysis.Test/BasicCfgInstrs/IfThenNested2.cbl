       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFTHEN.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(2) VALUE 10.
       01 B PIC 9(2) VALUE 20.
       01 C PIC 9(2) VALUE 30.
       PROCEDURE DIVISION.
           IF A = 10 THEN
             DISPLAY "A = 10"
             DISPLAY "RIGHT ?"
             IF B = 20 THEN
                DISPLAY "B = 20"
                DISPLAY "INSIDE A"
                IF C = 30 THEN
                    DISPLAY "C = 30"
                    DISPLAY "INSIDE B"
                END-IF
                DISPLAY "TEST C DONE"
             END-IF
             DISPLAY "TEST B DONE"
           END-IF.
           DISPLAY "TEST A DONE".
       END PROGRAM IFTHEN.
      