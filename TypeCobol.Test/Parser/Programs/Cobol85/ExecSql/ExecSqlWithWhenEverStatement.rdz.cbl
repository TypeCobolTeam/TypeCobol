       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           EXEC SQL
              WHENEVER SQLERROR GOTO ENDDATA
           END-EXEC
           EXEC SQL
              WHENEVER SQLWARNING CONTINUE
           END-EXEC
           EXEC SQL
              WHENEVER NOT FOUND GO TO : LabelName
           END-EXEC
           EXEC SQL
              WHENEVER NOT FOUND GO TO
           END-EXEC
           EXEC SQL
              WHENEVER CONTINUE
           END-EXEC
           EXEC SQL
              WHENEVER GOTO
           END-EXEC
           GOBACK
           .
       END PROGRAM DVZZMFT3.