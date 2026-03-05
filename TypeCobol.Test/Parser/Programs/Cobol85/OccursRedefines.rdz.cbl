       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOCCLVL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ROOT-GRP.
      * Error: a redefined data cannot be an OCCURS
           05 VAR-OCC PIC X(10) OCCURS 10.
           05 GRP-REDEF REDEFINES VAR-OCC.
              10 VAR1 PIC X(100).

       01 ROOT-GRP2.
      * Error: a redefined data cannot be an OCCURS
           05 VAR-OCC2 PIC X(10) OCCURS 10.
           05 GRP-REDEF2 REDEFINES VAR-OCC2 PIC X(100).

      * OK
       01 ROOT-GRP3.
           05 VAR-OCC3.
             10 POSTE3 PIC X(10) OCCURS 10.
           05 GRP-REDEF3 REDEFINES VAR-OCC3 PIC X(100).

       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOCCLVL.