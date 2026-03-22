       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTINSRT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NAME          PIC X(30).
       01  WS-AGE           PIC 9(3).
       01  WS-SALARY        PIC 9(7)V99.
       PROCEDURE DIVISION.
      * Simple INSERT with host variables
           EXEC SQL
              INSERT INTO EMPLOYEE (NAME, AGE)
              VALUES (:WS-NAME, :WS-AGE)
           END-EXEC
      * INSERT with schema-qualified table
           EXEC SQL
              INSERT INTO HR.EMPLOYEE (SALARY)
              VALUES (:WS-SALARY)
           END-EXEC
      * INSERT with subselect
           EXEC SQL
              INSERT INTO ARCHIVE.EMPLOYEE (NAME, AGE)
              SELECT NAME, AGE FROM EMPLOYEE
           END-EXEC
      * INSERT without column list
           EXEC SQL
              INSERT INTO EMPLOYEE
              VALUES (:WS-NAME, :WS-AGE, :WS-SALARY)
           END-EXEC
           GOBACK.
       END PROGRAM TSTINSRT.
