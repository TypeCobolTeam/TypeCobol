       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         EXEC SQL 
        SELECT i.i.d.* , e.i.* FROM a
         END-EXEC.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM DVZZMFT3.