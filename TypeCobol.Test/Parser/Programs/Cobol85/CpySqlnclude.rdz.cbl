       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *No error because there is no level 01
           EXEC SQL INCLUDE YMyCpy
           END-EXEC.
       01 var2 pic X.
       procedure division.
      *No error
           move MyCpy to var2
           goback
           .
       END PROGRAM MyPGM.