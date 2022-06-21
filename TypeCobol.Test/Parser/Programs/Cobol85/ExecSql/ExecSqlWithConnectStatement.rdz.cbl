       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              CONNECT
              CONNECT RESET
              CONNECT TO EASTDB
              CONNECT TO :LOC USER :AUTHID USING :PASSWORD
              CONNECT RESET TO
              CONNECT USER :UserName USING :UserPassword
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.