       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
              SELECT * FROM a
           END-EXEC.
           EXEC SQL
              SELECT ALL * FROM a
           END-EXEC.
           EXEC SQL
              SELECT DISTINCT * FROM a
           END-EXEC.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM DVZZMFT3.