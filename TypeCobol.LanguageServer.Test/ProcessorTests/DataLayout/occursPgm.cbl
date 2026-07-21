       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOCCURS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 WK-MAIN.
      * No OCCURS
           05 T-SIZE PIC S9(04) COMP.
           05 GRP.
              10 GRP-VAR PIC X.
      * OCCURS 1
           05 T1 OCCURS 1.
              10 T1-VAR PIC X.
      * Nested OCCURS
           05 T2 OCCURS 2.
              10 T2-NESTED1 OCCURS 3.
                 15 T2-NESTED1-NESTED OCCURS 5.
                    20 T2-NESTED1-NESTED-VAR PIC X OCCURS 7.
              10 T2-NESTED2 OCCURS 11.
                 15 T2-NESTED2-VAR PIC X.
      * Min and Max OCCURS
           05 T-MIN OCCURS 1 TO 99 DEPENDING ON T-SIZE.
              10 T-MIN-VAR PIC X.

       LINKAGE SECTION.
        01 LK-MAIN.
      * Unbounded OCCURS -> Max OCCURS = 1
           05 T-UNBOUNDED OCCURS 1 TO UNBOUNDED
                                      DEPENDING ON T-SIZE.
              10 T-UNBOUNDED-VAR PIC X.

       END PROGRAM TCOCCURS.