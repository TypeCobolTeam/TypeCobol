       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              WHENEVER SQLERROR GOTO ENDDATA  
              WHENEVER SQLWARNING CONTINUE
              WHENEVER NOT FOUND GO TO : LabelName
              WHENEVER NOT FOUND GO TO 
              WHENEVER CONTINUE
              WHENEVER GOTO
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.