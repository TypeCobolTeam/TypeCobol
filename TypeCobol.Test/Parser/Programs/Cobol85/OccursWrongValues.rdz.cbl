       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOCCLVL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ROOT-GRP.
          05 CNT PIC S9(2) BINARY.
      *    OK  
	      05 MAX-OK OCCURS 5 PIC X. 
      *    KO: Max value cannot be zero or negative
          05 MAX-ZERO   OCCURS 0      PIC X.
          05 MAX-NEG    OCCURS -2     PIC X.
      *    OK: Min value can be zero
          05 MIN-ZERO OCCURS 0 TO 5 DEPENDING ON CNT PIC X.
      *    KO: Min value cannot be negative
          05 MIN-NEG OCCURS -2 TO 1 DEPENDING ON CNT PIC X.
      *    KO: Min value cannot exceed Max value
          05 MIN-EXCEED-MAX OCCURS 2 TO 1 DEPENDING ON CNT PIC X.

       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOCCLVL.