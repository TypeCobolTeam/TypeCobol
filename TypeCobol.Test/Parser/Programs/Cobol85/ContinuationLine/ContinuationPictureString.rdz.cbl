       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT0.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Testing various interrupted PIC/usage clauses, expect no errors
       01 group1.
          05 data1                                                 PIC X
      -    X.
          05 data2                                                PIC X(
      -    1).
          05 data3                                          PIC 9(4) COM
      -    P-3.
          05 data4                                         PIC 9(4) COMP
      -    -3.
          05 data5                                        PIC 9(4) COMP-
      -    3.
       01 group2.
          05 data1 PIC X
      -    X.
          05 data2 PIC X(
      -    1).
          05 data3 PIC 9(4) COM
      -    P-3.
          05 data4 PIC 9(4) COMP
      -    -3.
          05 data5 PIC 9(4) COMP-
      -    3.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM DVZZMFT0.