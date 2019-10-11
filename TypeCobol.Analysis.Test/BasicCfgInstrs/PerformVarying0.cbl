       IDENTIFICATION DIVISION.
         PROGRAM-ID. PERFORMVRA.
      
       DATA DIVISION.
         WORKING-STORAGE SECTION.
           77 n   PIC 99.
           77 counter PIC 99.
      
       PROCEDURE DIVISION.
           DISPLAY "n : " NO ADVANCING ACCEPT n
           PERFORM VARYING counter FROM 1 BY 2 UNTIL counter > n
             DISPLAY counter " ; " NO ADVANCING
           END-PERFORM
           STOP RUN.
       END PROGRAM PERFORMVRA.
      