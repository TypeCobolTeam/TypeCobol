       IDENTIFICATION DIVISION.
       program-id. DVZZMFT3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.
       data division.
       working-storage section.
       01 SALARY       PIC S9(09) COMP.
       01 COMMISSION   PIC S9(09) COMP.
       01 COMMISSION_1 PIC S9(09).
       01 SALARY_1     PIC S9(09) COMP.
       01 SALARY_2     PIC S9(09) COMP.
       01 var          pic X(10).
       01 var2         pic X(10).
           
       procedure division.
      *      KO
      *DSNH104I DSNHPARS LINE 44 COL 16  ILLEGAL
      *SYMBOL"<END-OF-STATEMEN". SOME SYMBOLS THAT MIGHT BE LEGAL ARE:
      *CONNECTION SCHEMA ( :<IDENTIFIER>CURRENT CURRENT_L
             EXEC SQL SET
             END-EXEC.
      *      KO
      *DSNH104I DSNHPARS LINE 48 COL 21  ILLEGAL
      * SYMBOL"<END-OF-STATEMENT>". SOME SYMBOLS THAT MIGHT BE LEGAL
      * ARE: . : INDICATOR = [ ??(
             EXEC SQL SET :var2
             END-EXEC.
             EXEC SQL SET :SALARY = 50000
             END-EXEC.
             EXEC SQL SET :SALARY = NULL
             END-EXEC.
             EXEC SQL SET :SALARY = 50000, :SALARY_1 = 50000
             END-EXEC.
      *KO       
      *DSNH199I DSNHSM5R LINE 59 COL 95  INVALID KEYWORD"DEFAULT"; VALID
      *  SYMBOLS ARE:<EXPRESSION>,NULL,CURRENT SERVER,CURRENT PACKAGESET
             EXEC SQL SET :SALARY = DEFAULT,
              (:SALARY, :COMMISSION) = (50000, 8000)
             END-EXEC.
             EXEC SQL SET (:SALARY, :COMMISSION) = (50000, 8000)
             END-EXEC.
             EXEC SQL SET (:SALARY, COMMISSION) = (50000, 8000)
             END-EXEC.
             EXEC SQL SET (:SALARY, :COMMISSION) = (VALUES(NULL, 8000))
             END-EXEC.
      *KO
      *SQL host variable reference"COMMISSION_1"had invalid syntax or
      * the referenced host variable was an invalid SQL variable type.
      * The statement was discarded.
             EXEC SQL SET (:SALARY_1, :COMMISSION_1) = (50000, 8000),
                   :SALARY_2 = 50000
             END-EXEC.
             EXEC SQL SET (:var) = (VALUES 499)
             END-EXEC.
             EXEC SQL SET (:SALARY, :COMMISSION) = (5)
             END-EXEC.
      *KO
      *DSNH199I DSNHSM5R LINE 75 COL 114  INVALID KEYWORD"DEFAULT";
      *VALID SYMBOLS ARE:<EXPRESSION>,NULL,CURRENT SERVER,CURRENT
      *PACKAGESET
             EXEC SQL SET (:SALARY, :COMMISSION) = (5, DEFAULT)
             END-EXEC.
      *KO
      *DSNH104I DSNHPARS LINE 77 COL 111  ILLEGAL SYMBOL",". SOME
      *SYMBOLS THAT MIGHT BE LEGAL ARE: VALUES SELECT DEFAULT ( ? TRIM
      *XMLSERIALIZE CAST XMLCAST CASE
             EXEC SQL SET (:SALARY, :COMMISSION) = (,)
             END-EXEC.
      *KO
      *SQL host variable reference"COMMISSION_1"had invalid syntax or
      *the referenced host variable was an invalid SQL variable type.
      *The statement was discarded.
             EXEC SQL SET (:SALARY_1, :COMMISSION_1) = (50000, 8000) ,
                 :SALARY_2 = 50000,
                (:SALARY, :COMMISSION) = (VALUES(NULL, 8000))
             END-EXEC.
             EXEC SQL set SALARY = 2 * COMMISSION
             END-EXEC.
             EXEC SQL SET :var2 = :var
             END-EXEC.
           goback.
       end program DVZZMFT3.