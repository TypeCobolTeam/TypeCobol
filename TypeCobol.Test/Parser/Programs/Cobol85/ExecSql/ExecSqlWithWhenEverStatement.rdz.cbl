       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              WHENEVER SQLERROR GOTO ENDDATA         
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.