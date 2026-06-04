       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOZERRSY.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 ROOT-GROUP.
          05 VAR-A PIC X.
          05 GROUP-A.
             10 VAR-B PIC X.
             10 GROUP-B OCCURS 10 TIMES.
                15 VAR-C PIC X.
                15 GROUP-C.
                   20 VAR-D PIC X.
      * Not supported in DataLayout: SYNC included in an OCCURS
                   20 SYNCED-VAR  PIC S9(3) COMP SYNC.
                   20 VAR-E PIC X.

       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOZERRSY.