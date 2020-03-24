      *TypeCobol_Version:0.1(alpha)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EncodingTest.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 literal PIC X(10) VALUE "�-�-�-�".
       PROCEDURE DIVISION.
           GOBACK.
           .
       END PROGRAM EncodingTest.
