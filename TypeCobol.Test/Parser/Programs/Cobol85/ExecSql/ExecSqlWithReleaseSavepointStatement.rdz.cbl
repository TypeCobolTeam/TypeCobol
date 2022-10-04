       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           EXEC SQL 
              RELEASE SAVEPOINT A
           END-EXEC
           EXEC SQL
              RELEASE TO SAVEPOINT B
           END-EXEC
           EXEC SQL
              RELEASE SAVEPOINT             
           END-EXEC
           GOBACK
           .
       END PROGRAM DVZZMFT3.