       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 pic x.
       01 var2 pic x.
       01 var3 pic x.
       PROCEDURE DIVISION.
       REPLACE ==:debug-code:== BY ==
           DISPLAY var1
           DISPLAY var2
           DISPLAY var3
           ==.
           :debug-code:
           GOBACK
           .
       END PROGRAM TCOMFL06.