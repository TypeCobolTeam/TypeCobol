       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVAL.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 menu-input PIC X(1) VALUE "R".
       PROCEDURE DIVISION.
           EVALUATE menu-input
             WHEN 'A'
             WHEN 'B'
             WHEN 'C'
             WHEN 'D'
               DISPLAY "OPTION [A-B]"
             WHEN "E"
               DISPLAY "OPTION E"
             WHEN "F"
             WHEN "G"
               DISPLAY "OPTION F and G"
             WHEN OTHER
                DISPLAY "BAD OPTION"
           END-EVALUATE.
       END PROGRAM EVAL.
      