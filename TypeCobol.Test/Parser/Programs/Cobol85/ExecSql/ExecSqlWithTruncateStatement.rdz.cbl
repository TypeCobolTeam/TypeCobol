       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              TRUNCATE TABLE INVENTORY
              REUSE STORAGE
              IGNORE DELETE TRIGGERS

              TRUNCATE TABLE INVENTORY
              DROP STORAGE 
              RESTRICT WHEN DELETE TRIGGERS
              IMMEDIATE

              TRUNCATE TABLE INVENTORY
              REUSE STORAGE
              RESTRICT WHEN DELETE TRIGGERS            
              
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.