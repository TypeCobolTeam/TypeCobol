       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTCURS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ID            PIC 9(5).
       PROCEDURE DIVISION.
      * Simple DECLARE CURSOR
           EXEC SQL
              DECLARE C1 CURSOR FOR
              SELECT NAME, AGE FROM EMPLOYEE
           END-EXEC
      * DECLARE CURSOR WITH HOLD
           EXEC SQL
              DECLARE C2 CURSOR WITH HOLD FOR
              SELECT NAME FROM HR.EMPLOYEE
           END-EXEC
      * DECLARE CURSOR WITH RETURN
           EXEC SQL
              DECLARE C3 CURSOR WITH RETURN FOR
              SELECT SALARY FROM EMPLOYEE
           END-EXEC
      * DECLARE CURSOR for a prepared statement name
           EXEC SQL
              DECLARE C4 CURSOR FOR PREP_STMT1
           END-EXEC
           GOBACK.
       END PROGRAM TSTCURS.
