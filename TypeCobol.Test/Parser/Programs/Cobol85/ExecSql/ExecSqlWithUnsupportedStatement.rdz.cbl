       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTUNSQL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VAR1        PIC X(10).
       01  WS-VAR2        PIC 9(5).
       PROCEDURE DIVISION.
      * Supported SQL - should parse normally
           EXEC SQL
              COMMIT
           END-EXEC
      * Unsupported: OPEN cursor
           EXEC SQL
              OPEN C1
           END-EXEC
      * Unsupported: FETCH
           EXEC SQL
              FETCH C1 INTO :WS-VAR1, :WS-VAR2
           END-EXEC
      * Unsupported: CLOSE cursor
           EXEC SQL
              CLOSE C1
           END-EXEC
      * Supported SQL after unsupported - should still parse
           EXEC SQL
              ROLLBACK
           END-EXEC
           GOBACK.
       END PROGRAM TSTUNSQL.
