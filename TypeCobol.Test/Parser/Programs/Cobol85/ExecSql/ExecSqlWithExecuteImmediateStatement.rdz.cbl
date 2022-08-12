       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.
       data division.
       working-storage section.
       01 var1 pic X(50).
       01 var1b.
           49 var1b-LENGTH  PIC S9(4)       COMP-4 VALUE +150.
           49 var1b-VALUE   PIC X(150).
       01 var2 pic X(50).
       01 var3 pic X(50).

        EXEC SQL INCLUDE SQLCA
        END-EXEC.

       procedure division.
      *Ko
      *DSNH080I DSNHSM3D LINE 44 COL 35  STRING VARIABLE"H"IS
      *NOT"VARCHAR"TYPE
             EXEC SQL EXECUTE IMMEDIATE :var1
             END-EXEC.
      *Ok var1b is declared as varchar
             EXEC SQL EXECUTE IMMEDIATE :var1b
             END-EXEC.
      *Ko
      *DSNH199I DSNHPARS LINE 48 COL 105  INVALID KEYWORD"INDICATOR";
      *VALID SYMBOLS ARE: .<END-OF-STATEMENT>
             EXEC SQL EXECUTE IMMEDIATE :var2 INDICATOR :var3
             END-EXEC.
      *Ok
             EXEC SQL EXECUTE IMMEDIATE 'SELECT * FROM table1'
             END-EXEC.
      *Ko
      *DSNH104I DSNHPARS LINE 52 COL 34  ILLEGAL SYMBOL"555". SOME
      *SYMBOLS THAT MIGHT BE LEGAL ARE:<IDENTIFIER>:<PLI_STRING>
             EXEC SQL EXECUTE IMMEDIATE 555
             END-EXEC.
      *Ko
             EXEC SQL EXECUTE IMMEDIATE
             END-EXEC.
             
             goback
           .
       END PROGRAM DVZZMFT3.