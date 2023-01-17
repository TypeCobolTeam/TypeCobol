       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       replace ==:A:== by ==01==.
       01 var1 pic X(:A:).
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM MyPgm.