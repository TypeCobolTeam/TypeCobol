       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOZEFR3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01 included-copy. COPY INCLUDED.
       01 grp-root.
          05 grp-simple.
             10 var-simple PIC X.
             10 var-simple-redef REDEFINES var-simple PIC X.
             10 grp-target.
                15 var1 PIC X.
             10 grp-redef REDEFINES grp-target.
                15 var2 PIC X.
          05 grp-dim.
             10 grp-one-dim OCCURS 10.
                15 var-one-dim PIC X.
                15 var-one-dim-redef REDEFINES var-one-dim PIC X.
                15 grp-two-dim OCCURS 10.
                   20 var-two-dim PIC X.
                   20 grp-three-dim OCCURS 10.
                      25 var-three-dim PIC X.
                15 var-inline-two-dim PIC X(2) OCCURS 10.
          05 grp-picture.
             10 var-x PIC xx.
             10 var-9 PIC 9(2).
             10 var-national PIC N(2)N.
             10 var-national-edited PIC BN.
             10 FILLER PIC X.
             10 FILLER PIC 9.
          05 grp-picture-not-displayable.
             10 FILLER PIC N.
             10 PIC BN.
          05 grp-usage.
             10 var-none PIC X.
             10 var-display PIC X USAGE DISPLAY.
             10 var-comp PIC 9 USAGE COMP.
             10 var-comp5 PIC 9 USAGE COMP-5.
             10 var-pointer USAGE POINTER.
          05 grp-usage-not-displayable.
             10 var-index USAGE INDEX.
             10 var-index-redef REDEFINES var-index USAGE INDEX.
             10 var-function-pointer USAGE FUNCTION-POINTER.
             10 var-procedure-pointer USAGE PROCEDURE-POINTER.
          05 var-occurs OCCURS 65535 PIC X.
          05 var-occurs-not-displayable OCCURS 65536 PIC X.
          05 grp-occurs-not-displayable OCCURS 65536.
             10 var1-not-displayable PIC X.
             10 grp1-not-displayable.
                15 var2-not-displayable PIC X.

       END PROGRAM TCOZEFR3.