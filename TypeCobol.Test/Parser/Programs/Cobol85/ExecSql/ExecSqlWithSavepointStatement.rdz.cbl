       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           EXEC SQL
              SAVEPOINT B UNIQUE
              ON ROLLBACK RETAIN CURSORS
           END-EXEC
           EXEC SQL
              SAVEPOINT A
              ON ROLLBACK RETAIN CURSORS
              ON ROLLBACK RETAIN LOCKS
           END-EXEC
           EXEC SQL
              SAVEPOINT B UNIQUE
              ON ROLLBACK RETAIN LOCKS
              ON ROLLBACK RETAIN CURSORS
           END-EXEC
           EXEC SQL
              SAVEPOINT B UNIQUE
           END-EXEC
           EXEC SQL
              SAVEPOINT sYst UNIQUE
              ON ROLLBACK RETAIN CURSORS
           END-EXEC
           GOBACK
           .
       END PROGRAM DVZZMFT3.