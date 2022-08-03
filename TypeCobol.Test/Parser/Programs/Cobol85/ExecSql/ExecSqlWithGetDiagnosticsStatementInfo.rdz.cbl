       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.
       data division.
       working-storage section.


       01 var1 pic X(10).
       01 var2 pic X(10).
       01 var01 pic X(10).
       01 var02 pic X(10).
       01 var03 pic X(10).
       01 var04 pic X(10).
       01 var05 pic X(10).
       01 var06 pic X(10).
       01 var07 pic X(10).
       01 var08 pic X(10).
       01 var09 pic X(10).
       01 var10 pic X(10).
       01 var11 pic X(10).
       01 var12 pic X(10).
       01 var13 pic X(10).

           EXEC SQL INCLUDE SQLCA
           END-EXEC.

       procedure division.
           EXEC SQL
              GET DIAGNOSTICS :var1 = DB2_GET_DIAGNOSTICS_DIAGNOSTICS
           END-EXEC.

           EXEC SQL
              GET DIAGNOSTICS :var1 = DB2_SQL_NESTING_LEVEL
           END-EXEC.

           EXEC SQL
              GET DIAGNOSTICS
                    :var01 = DB2_LAST_ROW,
                    :var02 = DB2_NUMBER_PARAMETER_MARKERS,
                    :var03 = DB2_NUMBER_RESULT_SETS,
                    :var04 = DB2_NUMBER_ROWS,
                    :var05 = DB2_RETURN_STATUS,
                    :var06 = DB2_SQL_ATTR_CURSOR_HOLD,
                    :var07 = DB2_SQL_ATTR_CURSOR_ROWSET,
                    :var08 = DB2_SQL_ATTR_CURSOR_SCROLLABLE,
                    :var09 = DB2_SQL_ATTR_CURSOR_SENSITIVITY,
                    :var10 = DB2_SQL_ATTR_CURSOR_TYPE,
                    :var11 = MORE,
                    :var12 = NUMBER,
                    :var13 = ROW_COUNT
           END-EXEC.

           EXEC SQL GET DIAGNOSTICS :var1 = DB2_LAST_ROW
           END-EXEC.

      *KO
      *It should works according to specifications
      *But it produces error
      *DSNH104I DSNHPARS ILLEGAL SYMBOL"<END-OF-STATEMENT>".
      *SOME SYMBOLS THAT MIGHT BE LEGAL ARE: = .
           EXEC SQL GET DIAGNOSTICS :var1 = DB2_LAST_ROW, MORE
           END-EXEC.


           EXEC SQL
              GET CURRENT DIAGNOSTICS
                :var1 = DB2_GET_DIAGNOSTICS_DIAGNOSTICS
           END-EXEC.

      *KO
      *It should works according to specifications
      *But it produces error
      *DSNH104I DSNHPARS ILLEGAL SYMBOL"<END-OF-STATEMENT>".
      *SOME SYMBOLS THAT MIGHT BE LEGAL ARE: = .
           EXEC SQL GET CURRENT  DIAGNOSTICS :var1 = DB2_LAST_ROW, MORE
           END-EXEC.


      *KO
      *DSNH20228I DSNHSM1A A STACKED DIAGNOSTICS AREA IS NOT AVAILABLE
           EXEC SQL GET STACKED DIAGNOSTICS :var1 = DB2_LAST_ROW
           END-EXEC.

      *KO
      *DSNH20228I DSNHSM1A A STACKED DIAGNOSTICS AREA IS NOT AVAILABLE
           EXEC SQL
             GET STACKED DIAGNOSTICS :var1 = DB2_LAST_ROW, :var2 =MORE
           END-EXEC.

           goback.
       END PROGRAM DVZZMFT3.