       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Group1.
           05 Array1 OCCURS 60 TIMES
                              INDEXED BY MyIndex.
             10 Array1-Elt1             PIC X(03).

       PROCEDURE DIVISION.
           SEARCH Array1 VARYING MyIndex
           at end


           END-SEARCH
           .

       MyParagraph.
           display "Foo"

           .
       END PROGRAM MyPGM.