       IDENTIFICATION DIVISION.
       PROGRAM-ID. RB1BCTLB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 W-TAB.
          05 W-TAB2 OCCURS 50 INDEXED BY I-DTL.
            10 W-TAB-ELT                  PIC X(07).
       01 I-TAB-MAX                           PIC S9(02) COMP-5.
      
       PROCEDURE DIVISION.
           SET I-TAB-MAX                 UP TO 1
           .
       END PROGRAM RB1BCTLB.