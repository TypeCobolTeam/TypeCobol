       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.
       ENVIRONMENT DIVISION.
      *Ok
       CONFIGURATION SECTION.
       DATA DIVISION.
       PROCEDURE DIVISION.

           goback.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested.
       ENVIRONMENT DIVISION.
      *Ko CONFIGURATION SECTION is not allowed in nested programs
       CONFIGURATION SECTION.
       PROCEDURE DIVISION.
           goback.
       END PROGRAM Nested.

       END PROGRAM Main.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacked.
       ENVIRONMENT DIVISION.
      *Ok for stacked
       CONFIGURATION SECTION.
       PROCEDURE DIVISION.
           goback.
       END PROGRAM Stacked.
