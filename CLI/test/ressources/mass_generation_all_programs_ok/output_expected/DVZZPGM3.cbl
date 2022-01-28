      *TypeCobol_Version:v0.0.0-local
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZPGM3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC 9 VALUE '3'.
      
       procedure division.
           display "Inside PGM" var1
           goback
           .
       END PROGRAM DVZZPGM3.
