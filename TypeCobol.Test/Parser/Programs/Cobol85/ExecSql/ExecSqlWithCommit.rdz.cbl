       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         EXEC SQL DELETE FROM CUSTOMERS	   
         COMMIT
         END-EXEC.
       PROCEDURE DIVISION.
           EXEC SQL COMMIT 
           END-EXEC 
           display "hello" 
           EXEC SQL COMMIT      
           END-EXEC
           EXEC SQL COMMIT WORK 
           END-EXEC
           GOBACK
           .
       END PROGRAM DVZZMFT3.