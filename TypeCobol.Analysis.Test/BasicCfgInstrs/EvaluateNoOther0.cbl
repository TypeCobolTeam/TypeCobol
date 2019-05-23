       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVAL.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 menu-input PIC X(1) VALUE "R".
       01 Grp.
           05 i PIC 9(4).
       PROCEDURE DIVISION.
           EVALUATE menu-input
             WHEN "0"
               DISPLAY "OPTION 0"
             WHEN "1" THRU "9"
               DISPLAY "OPTION 1-9"
             WHEN "R"
               DISPLAY "OPTION R"
             WHEN "X"
               DISPLAY "OPTION X"
           END-EVALUATE
           DISPLAY "MAYBE NO OPTIONS".
       END PROGRAM EVAL.
      