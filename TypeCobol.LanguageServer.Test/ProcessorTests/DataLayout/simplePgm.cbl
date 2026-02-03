       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOZEFR0.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           .
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01. 02 occurs 1.
           05 Array1 occurs 9.
             10 Var1 pic X.
             10 Array2 occurs 8.
               15 Var2 pic X.
       01  .
           05 GRP-REDEFINED.
              10 VAR-1                      PIC  9(05)      COMP-3.
           05 FILLER                        REDEFINES GRP-REDEFINED.
              10 VAR-RED1                   PIC  9(05)      COMP-3.
           05 GROUP-WITH-88-1.
             10 GROUP-WITH-88-2             PIC  9(01).
                88 CONDITION1               VALUE 0.
                88 CONDITION2               VALUE 1.

       LOCAL-STORAGE SECTION.
       01 Var1-in-ls pic XX.
       01 num1         pic 9(03).
       01 num2         comp-1.
       01 num3         comp-2.
       01 num4         pic 9(03) comp-3.
       01 num5         pic 9(03) comp-4.
       01 num6         pic 9(03) comp-5.
       01 num7         pic 9(03) packed-decimal.
       01 num8         pic 9(03) binary.
       01 num9         pic 9(03) comp.

       LINKAGE SECTION.
       01 Var2-in-lk pic X(2).

       END PROGRAM TCOZEFR0.