
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       COPY "copy.inc"
            REPLACING LEADING ==TEST== BY ==FIRST==
                      LEADING ==NORM== BY ==SECOND==.
       PROCEDURE        DIVISION.
           DISPLAY FIRST-VAR NO ADVANCING
           END-DISPLAY.
           DISPLAY SECOND-VAR NO ADVANCING
           END-DISPLAY.
           STOP RUN.
