      *TypeCobol_Version:v0.0.0-local
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EncodingTest.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 literal PIC X(10) VALUE "é-à-è-€".
       PROCEDURE DIVISION.
           GOBACK.
           .
       END PROGRAM EncodingTest.
