       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTUPDT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NAME          PIC X(30).
       01  WS-AGE           PIC 9(3).
       01  WS-ID            PIC 9(5).
       PROCEDURE DIVISION.
      * Simple UPDATE with host variable in SET and WHERE
           EXEC SQL
              UPDATE EMPLOYEE
              SET NAME = :WS-NAME
              WHERE ID = :WS-ID
           END-EXEC
      * UPDATE with multiple SET clauses
           EXEC SQL
              UPDATE HR.EMPLOYEE
              SET NAME = :WS-NAME, AGE = :WS-AGE
              WHERE ID = :WS-ID
           END-EXEC
      * UPDATE without WHERE clause
           EXEC SQL
              UPDATE EMPLOYEE
              SET AGE = :WS-AGE
           END-EXEC
           GOBACK.
       END PROGRAM TSTUPDT.
