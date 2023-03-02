       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       REPLACE ==:debug-code:== BY ==
           DISPLAY var1
           ==.
           :debug-code:
       REPLACE OFF.
           GOBACK
           .
       END PROGRAM TCOMFL06.