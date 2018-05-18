       ID DIVISION.
       PROGRAM-ID.     RPTCPY01
      *REMARKS. COPY=( YINIPRG
      *              ).
      *=========================
       DATA DIVISION.
      *=========================
       WORKING-STORAGE SECTION.
      *=========================
       01  YINIPRG.         COPY YINIPRG SUPPRESS.
       01  NOMPRG           PIC X(08) VALUE 'RPTCPY'.
       77  I                PIC 9(03) COMP-3.
       01  SELECT-CONSO     PIC X(03).
           88 SELECT-CA-OK            VALUE 'CA '.
           88 SELECT-CTR-OK           VALUE 'CTR'.
           88 SELECT-CJM-OK           VALUE 'CJM'.
       PROCEDURE DIVISION.
      *=========================
       debut_prgs.
           Initialize YINIPRG
           Move When-Compiled          to PRG-KOMPIL
           Move NOMPRG                 to PRG-PGMNAME
           Move 0 to I
           INITIALIZE SELECT-CONSO
           .
       END PROGRAM
