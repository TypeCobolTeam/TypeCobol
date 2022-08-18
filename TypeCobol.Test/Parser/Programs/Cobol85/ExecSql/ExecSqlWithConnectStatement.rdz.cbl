       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           EXEC SQL 
              CONNECT
           END-EXEC
           EXEC SQL
              CONNECT RESET
           END-EXEC
           EXEC SQL
              CONNECT TO EASTDB
           END-EXEC
           EXEC SQL
              CONNECT TO :LOC USER :AUTHID USING :PASSWORD
           END-EXEC
           EXEC SQL
              CONNECT RESET TO
           END-EXEC
           EXEC SQL
              CONNECT USER :UserName USING :UserPassword
           END-EXEC
           EXEC SQL
              CONNECT TO :LOC2
           END-EXEC
           EXEC SQL
              CONNECT TO LOC3 USER :AUTHID3 USING :PASSWORD3
           END-EXEC
           GOBACK
           .
       END PROGRAM DVZZMFT3.