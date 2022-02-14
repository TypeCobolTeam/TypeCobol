       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 item PIC 9(2).
          88 range1 VALUE 01 THRU
                          05.
          88 range2 VALUE 07 THRU
                          15
                          17 THRU
                          25.
       procedure division.
           goback
           .
       END PROGRAM MyPGM.