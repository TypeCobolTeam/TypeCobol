       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTDEL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ID            PIC 9(5).
       PROCEDURE DIVISION.
      * DELETE with WHERE clause and host variable
           EXEC SQL
              DELETE FROM EMPLOYEE
              WHERE ID = :WS-ID
           END-EXEC
      * DELETE with schema-qualified table
           EXEC SQL
              DELETE FROM HR.EMPLOYEE
              WHERE ID = :WS-ID
           END-EXEC
      * DELETE without WHERE clause
           EXEC SQL
              DELETE FROM EMPLOYEE
           END-EXEC
           GOBACK.
       END PROGRAM TSTDEL.
