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
               GET DIAGNOSTICS CONDITION 1
               :var01 = DB2_RETURNED_SQLCODE,
               :var02 = DB2_TOKEN_COUNT,
               :var03 = DB2_ORDINAL_TOKEN_1,
               :var04 = DB2_ORDINAL_TOKEN_2,
               :var05 = DB2_ORDINAL_TOKEN_3,
               :var06 = DB2_ORDINAL_TOKEN_4,
               :var07 = DB2_ORDINAL_TOKEN_5,
               :var08 = DB2_MESSAGE_ID,
               :var09 = MESSAGE_TEXT,
               :var10 = DB2_MODULE_DETECTING_ERROR,
               :var11 = RETURNED_SQLSTATE
           END-EXEC.
           EXEC SQL
              GET DIAGNOSTICS CONDITION
                              :var01 :var02 = RETURNED_SQLSTATE
           END-EXEC.
           EXEC SQL
              GET DIAGNOSTICS CONDITION
                              :var01 :var02 = DB2_PRODUCT_ID
           END-EXEC.
      *KO
      *DSNH104I DSNHPARS ILLEGAL SYMBOL"=". SOME SYMBOLS THAT MIGHT BE
      *LEGAL ARE: : .<IDENTIFIER>
           EXEC SQL
              GET DIAGNOSTICS CONDITION
                              :var01 = ALL STATEMENT
           END-EXEC.

           goback.
       END PROGRAM DVZZMFT3.