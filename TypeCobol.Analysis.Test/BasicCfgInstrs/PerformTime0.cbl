       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM0.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 phrase PIC 9 VALUE 0.
       77 c PIC 99.
       PROCEDURE DIVISION.
           DISPLAY "Enter : " NO ADVANCING ACCEPT phrase
           DISPLAY "Repeat : " NO ADVANCING ACCEPT c
           PERFORM c TIMES
             DISPLAY phrase
           END-PERFORM
           STOP RUN.
       END PROGRAM PERFORM0.
      