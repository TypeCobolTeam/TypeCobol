﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              ROLLBACK
              ROLLBACK TO SAVEPOINT A
              ROLLBACK WORK TO SAVEPOINT A
              ROLLBACK WORK
              ROLLBACK TO SAVEPOINT
              ROLLBACK WORK TO SAVEPOINT
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.