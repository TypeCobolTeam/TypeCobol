       IDENTIFICATION DIVISION.
       PROGRAM-ID. UnsupportedBySqlScanner.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE-FIN        PIC X(10).
       PROCEDURE DIVISION.
           EXEC SQL
               SELECT * FROM PROJECTS
      * KO: operator '<' is not supported by SQL scanner
               WHERE DATE_FIN < :WS-DATE-FIN
           END-EXEC
       END PROGRAM UnsupportedBySqlScanner.