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

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

       procedure division.
           EXEC SQL
              GET DIAGNOSTICS  :var01 = ALL CONDITION :var02
           END-EXEC.

           EXEC SQL
              GET DIAGNOSTICS  :var01 = ALL STATEMENT
           END-EXEC.

           EXEC SQL
              GET DIAGNOSTICS  :var01 = ALL STATEMENT, CONDITION,
                                            CONNECTION
           END-EXEC.

           EXEC SQL
              GET DIAGNOSTICS  :var01 = ALL
                       STATEMENT, CONDITION :var01, CONNECTION :var02
           END-EXEC.

      *Ok
           EXEC SQL
            GET DIAGNOSTICS  :var01 = ALL STATEMENT,
                                          CONDITION, CONNECTION 2
           END-EXEC.

      *Ok
           EXEC SQL
            GET DIAGNOSTICS  :var01 = ALL STATEMENT,
                                          CONDITION 3, CONNECTION
           END-EXEC.

      *KO
      *DSNH104I DSNHPARS ILLEGAL SYMBOL"2". SOME SYMBOLS THAT MIGHT BE
      *LEGAL ARE:<END-OF-STATEMENT>, .
           EXEC SQL
            GET DIAGNOSTICS  :var01 = ALL STATEMENT,
                                          CONDITION 3,CONNECTION 2
           END-EXEC.

      *KO
      *DSNH104I DSNHSM1A ILLEGAL SYMBOL"STATEMEN". SOME SYMBOLS THAT
      *MIGHT BE LEGAL ARE: DB2_LAST_ROW, DB2_NUMBER_RESULT_SETS,
      *DB2_SQL_ATTR_CURSOR_HO
           EXEC SQL
              GET DIAGNOSTICS  :var01 = STATEMENT,
                                        CONDITION, CONNECTION
           END-EXEC.

      *KO
      *DSNH104I DSNHSM1A ILLEGAL SYMBOL"ALL". SOME SYMBOLS THAT MIGHT BE
      *LEGAL ARE: DB2_LAST_ROW, DB2_NUMBER_RESULT_SETS,
      *DB2_SQL_ATTR_CURSOR_HOLD, ..
           EXEC SQL
              GET DIAGNOSTICS  :var01 = ALL
           END-EXEC.

      *KO
      *DSNH637I DSNHSM1A DUPLICATE"STATEMENT"KEYWORD OR CLAUSE.
           EXEC SQL
              GET DIAGNOSTICS  :var01 = ALL STATEMENT, STATEMENT
           END-EXEC.

           goback.
       END PROGRAM DVZZMFT3.