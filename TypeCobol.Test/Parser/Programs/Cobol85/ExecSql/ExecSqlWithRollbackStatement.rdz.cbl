       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              ROLLBACK
              ROLLBACK TO SAVEPOINT A
              ROLLBACK WORK TO SAVEPOINT A
              ROLLBACK WORK
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.