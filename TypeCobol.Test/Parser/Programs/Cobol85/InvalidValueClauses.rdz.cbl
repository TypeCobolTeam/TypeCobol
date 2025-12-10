       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group0.
          05 group1.
             10 var1 PIC X VALUE 'A'.
      * KO: VALUE used on a REDEFINES
             10 var-redef REDEFINES var1 PIC X VALUE 'B'.
          05 group2 REDEFINES group1.
      * KO: VALUE used on an item whose parent is a REDEFINES
             10 var2 PIC X VALUE 'C'.
          05 zone PIC X(10).
          05 group-redef REDEFINES zone.
             10 part1 PIC X(2).
             10 part2.
      * KO: VALUE used on an item located under a REDEFINES
                15 var3 PIC X(6) VALUE 'ABCDEF'.
             10 part3 PIC X(2).
      * OK: VALUE for conditions (level-88) are allowed
                88 flag-ok VALUE 'ok'.
                88 flag-ko VALUE 'ko'.
       01 group3 VALUE 'D'.
      * KO: VALUE used on an item whose parent has a VALUE clause
          05 var3 PIC X VALUE 'E'.
       01 group4 VALUE 'F'.
          05 group41.
      * KO: VALUE used on an item located under an item having a VALUE
             10 var5 PIC X VALUE 'G'.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.