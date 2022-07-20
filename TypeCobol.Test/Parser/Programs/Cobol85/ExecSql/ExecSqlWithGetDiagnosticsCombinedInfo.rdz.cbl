       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              GET DIAGNOSTICS  :var4 = ALL STATEMENT,
              CONDITION 3,CONNECTION    
              GET DIAGNOSTICS  :var4 = STATEMENT,
              CONDITION 3,CONNECTION    
              
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.