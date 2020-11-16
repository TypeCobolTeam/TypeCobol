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
                    DISPLAY "RIGHT ?"
                ELSE
                    DISPLAY "B <> 20"
                    DISPLAY "???"
                END-IF
             DISPLAY "AFTER NESTED B"
           ELSE
             DISPLAY "A <> 10"
             DISPLAY "???"
                IF C = 30 THEN
                    DISPLAY "C = 30"
                    DISPLAY "RIGHT ?"
                ELSE
                    DISPLAY "C <> 30"
                    DISPLAY "???"
                END-IF
             DISPLAY "AFTER NESTED C"
           END-IF.
           DISPLAY "AFTER A".
       END PROGRAM IFTHEN.
      