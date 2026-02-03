       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCHTEST.
       AUTHOR. MAYANJE
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  IDX-END      pic 9999 comp.
       01 LEVEL01.
         02 ELEM occurs 12
                INDEXED BY IDX.
           05 NUM     pic 99.
           05 NBJ     pic 99.
           05 LIB     pic x(9).
      
       PROCEDURE DIVISION.
           SET IDX to 1.
           SEARCH ELEM
                  when NBJ(IDX) = 28
                  display NUM(IDX) "/" LIB(IDX)
                  when IDX > IDX-END next sentence.
           display "------------------------------------".
           stop "Do Enter to continue".
       END PROGRAM SEARCHTEST.
      