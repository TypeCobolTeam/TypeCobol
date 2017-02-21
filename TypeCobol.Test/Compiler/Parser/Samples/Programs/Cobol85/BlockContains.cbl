      *Don't except any errors
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYPGM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       DATA DIVISION.
       FILE SECTION.
       FD F-ENT001                BLOCK CONTAINS 0
                                  RECORDING MODE IS F
                                  LABEL RECORD STANDARD.

       COPY NOTFOUND.

       FD F-ENT002                BLOCK CONTAINS 0
                                  LABEL RECORD STANDARD.

       01 ENT002-REC pic X.


       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM MYPGM.
