       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           EXEC SQL
              ROLLBACK
           END-EXEC
           EXEC SQL
              ROLLBACK TO SAVEPOINT A
           END-EXEC
           EXEC SQL
              ROLLBACK WORK TO SAVEPOINT A
           END-EXEC
           EXEC SQL
              ROLLBACK WORK
           END-EXEC
           EXEC SQL
              ROLLBACK TO SAVEPOINT
           END-EXEC
           EXEC SQL
              ROLLBACK WORK TO SAVEPOINT
           END-EXEC
           GOBACK
           .
       END PROGRAM DVZZMFT3.