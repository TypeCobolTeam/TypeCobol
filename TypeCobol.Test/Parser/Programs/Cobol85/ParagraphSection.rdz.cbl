       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFTIMI.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       PROCEDURE DIVISION.
       Section1 SECTION.
       PARA-7A.
           display "A"
           GO TO PARA-7B
           .
       PARA-xxxx.
           .
       PARA-7B.
           GOBACK
           .
       Section2 SECTION.
           .
       END PROGRAM PERFTIMI.