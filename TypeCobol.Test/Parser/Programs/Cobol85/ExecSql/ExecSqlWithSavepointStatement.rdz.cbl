       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              SAVEPOINT B UNIQUE                         
              ON ROLLBACK RETAIN CURSORS

              SAVEPOINT A 
              ON ROLLBACK RETAIN CURSORS
              ON ROLLBACK RETAIN LOCKS

              SAVEPOINT B UNIQUE 
              ON ROLLBACK RETAIN LOCKS
              ON ROLLBACK RETAIN CURSORS
         
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.