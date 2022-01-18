       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       REPLACE ==:T:==                 BY ==05  PIC X(70) VALUE ==
               .
       WORKING-STORAGE SECTION.
       01 W-TAB-ENT.
         :T: 'val'.
       END PROGRAM MyPGM.
