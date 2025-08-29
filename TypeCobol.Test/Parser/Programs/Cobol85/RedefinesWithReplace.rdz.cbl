       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOBCOMP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       REPLACE ==:zone:== BY ==zone1==.
       01 :zone: PIC X(120).
      * Undefined REDEFINES target, we get 2 diagnostics:
      * - one for the invalid parsing of the Redefines entry
      * - one for the invalid semantics of the REDEFINES
       01 :zone:-redef REDEFINES :zoneA:.
          05 z-label   PIC X(20).
          05 z-content PIC X(100).
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOBCOMP.