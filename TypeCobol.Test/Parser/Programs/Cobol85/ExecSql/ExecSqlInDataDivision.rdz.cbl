       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *KO empty group item
       01 group1.
          EXEC SQL
            SELECT * FROM Table2 
          END-EXEC.
      *KO same thing but with a copy
       01 group2.
       COPY CopyWithExecSqlOnly.
      *KO level number mismatch
       01 group3.
       COPY CopyWithExecSqlAndData1.
      *OK
       01 group4.
       COPY CopyWithExecSqlAndData2.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM DVZZMFT3.