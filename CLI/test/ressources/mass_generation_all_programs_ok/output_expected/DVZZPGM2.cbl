Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZPGM2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC 9 VALUE '2'.
      
       procedure division.
           display "Inside PGM" var1
           goback
           .
       END PROGRAM DVZZPGM2.
