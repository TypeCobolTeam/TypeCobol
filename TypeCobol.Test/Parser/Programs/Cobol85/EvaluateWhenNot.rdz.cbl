       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DVZF0OSM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A pic X.
       01 B pic X.
       PROCEDURE DIVISION.
           EVALUATE A ALSO B
           WHEN NOT(SPACE) ALSO SPACE
                display "case 1"
           WHEN SPACE ALSO NOT SPACE
                display "case 2"
           WHEN SPACE ALSO SPACE
                display "case 3"
           WHEN OTHER
                display "error"
           END-EVALUATE
           .
       END PROGRAM DVZF0OSM.