       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOCCLVL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * KO: OCCURS cannot be specified on level 01
        01 GRP01 OCCURS 2.
           05 VAR01 PIC X.
      * KO: OCCURS cannot be specified on level 77
        77 GRP77 PIC X(10) OCCURS 2.
      * OK
        01 GRPOK.
           05 VAROK PIC X OCCURS 2.

       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOCCLVL.