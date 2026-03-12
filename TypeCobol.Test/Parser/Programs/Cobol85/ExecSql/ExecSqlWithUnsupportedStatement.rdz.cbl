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
      * Unsupported: INSERT
           EXEC SQL
              INSERT INTO TABLE1 (COL1, COL2)
              VALUES (:WS-VAR1, :WS-VAR2)
           END-EXEC
      * Unsupported: UPDATE
           EXEC SQL
              UPDATE TABLE1
              SET COL1 = :WS-VAR1
              WHERE COL2 = :WS-VAR2
           END-EXEC
      * Unsupported: DELETE
           EXEC SQL
              DELETE FROM TABLE1
              WHERE COL1 = :WS-VAR1
           END-EXEC
      * Unsupported: DECLARE CURSOR
           EXEC SQL
              DECLARE C1 CURSOR FOR
              SELECT COL1, COL2 FROM TABLE1
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
