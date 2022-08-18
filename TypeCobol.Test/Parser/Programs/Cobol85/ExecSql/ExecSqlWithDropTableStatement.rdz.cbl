       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           EXEC SQL 
              DROP TABLE nom.h.r
           END-EXEC
           EXEC SQL
              DROP TABLE myTable
           END-EXEC
           EXEC SQL
              DROP TABLE
           END-EXEC
           EXEC SQL
              DROP TABLE a.b
           END-EXEC
           GOBACK
           .
       END PROGRAM DVZZMFT3.