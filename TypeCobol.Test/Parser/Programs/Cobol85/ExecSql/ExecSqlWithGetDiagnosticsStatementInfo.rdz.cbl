       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
             
              GET STACKED DIAGNOSTICS :var1 = DB2_LAST_ROW, MORE 
              GET STACKED DIAGNOSTICS :var1 = DB2_LAST_ROW, :var2 =MORE
              
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.