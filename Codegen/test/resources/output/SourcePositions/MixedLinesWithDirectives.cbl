       IDENTIFICATION DIVISION.
       PROGRAM-ID. MixedLinesWithDirectives.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       REPLACE ==:a:== by ==b==.
       DATA DIVISION. REPLACE ==:z:== by ==y==.

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.

       END PROGRAM MixedLinesWithDirectives.

