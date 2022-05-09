       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 item1 PIC 9(2).
          88 range1 VALUE 01 THRU
                          05.
          88 range2 VALUE 07 THRU
                          15
                          17 THRU
                          25.
       01 item2 PIC 9(2).
          88 test1 VALUE ZERO
                         01.
          88 test2 VALUE ZEROS
                         01.
          88 test3 VALUE ZEROES
                         01.
       procedure division.
           goback
           .
       END PROGRAM MyPGM.