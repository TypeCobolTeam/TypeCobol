       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         EXEC SQL 
        SELECT ensi.hend.laabidi.* , khaled.*
         END-EXEC.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM DVZZMFT3.