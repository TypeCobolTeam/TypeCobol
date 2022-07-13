       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
             EXECUTE IMMEDIATE :var1 
             EXECUTE IMMEDIATE :var2 INDICATOR :var3
             EXECUTE IMMEDIATE 'SELECT * FROM table1'
             EXECUTE IMMEDIATE 
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.