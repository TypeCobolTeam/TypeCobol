       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOOABCDE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      

       01 FOOFAW.                      COPY YFOOFAW SUPPRESS.
       01 FOOFRW.                      COPY YFOOFRW SUPPRESS.
       01 FOOHEW.                      COPY YFOOHEW SUPPRESS.
      
       01 C-FOOZGERR                   PIC X(8) VALUE 'FOOZGERR'.

       01 W-FCT-IDX                    PIC 9(2).
       77 W-FCT-IDX-MAX                PIC 9(2) VALUE 10.

       01  FOOT00.                     COPY YFOOT00.
       01  FOOT01.                     COPY YFOOT01 SUPPRESS.


       LOCAL-STORAGE SECTION.

       01  FOOT10.                     COPY YFOOT10 SUPPRESS.
       01  FOOT11.                     COPY YFOOT11 SUPPRESS.
       01  FOOT12.                     COPY YFOOT12 SUPPRESS.
       01  FOOT13.                     COPY YFOOT13 SUPPRESS.
       01  FOOT14.                     COPY YFOOT14 SUPPRESS.
       01  FOOT15.                     COPY YFOOT15 SUPPRESS.
      
       PROCEDURE DIVISION.
      
       END PROGRAM FOOABCDE.
