       IDENTIFICATION DIVISION.
       program-id. DVZZMFT3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.
       data division.
       working-storage section.
       procedure division.
           EXEC SQL ALTER SEQUENCE org_seq RESTART
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq RESTART WITH 100
           END-EXEC.
      *KO
      *DSNH336I DSNHSM5S THE SCALE OF THE DECIMAL NUMBER MUST BE ZERO
           EXEC SQL ALTER SEQUENCE org_seq RESTART WITH 100,1
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq INCREMENT BY 5
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq INCREMENT BY 5
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq NO MINVALUE
           END-EXEC.
      *KO
      *DSNH637I DSNHSM5S DUPLICATE "MINVALUE" KEYWORD OR CLAUSE.
           EXEC SQL ALTER SEQUENCE org_seq2 MINVALUE 6 MINVALUE 8
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq2 MINVALUE 6,0
           END-EXEC.
      *KO
      *DSNH336I DSNHSM5S THE SCALE OF THE DECIMAL NUMBER MUST BE ZERO
           EXEC SQL ALTER SEQUENCE org_seq2 MINVALUE 6,1
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq NO MAXVALUE
           END-EXEC.
      *KO
      *DSNH628I DSNHSM5S THE CLAUSES ARE MUTUALLY EXCLUSIVE
           EXEC SQL ALTER SEQUENCE org_seq2 MAXVALUE 25 NO MAXVALUE
           END-EXEC.
      *KO
      *DSNH336I DSNHSM5S THE SCALE OF THE DECIMAL NUMBER MUST BE ZERO
           EXEC SQL ALTER SEQUENCE org_seq2 MAXVALUE 7,1
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq2 MAXVALUE 7
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq2 NO CYCLE
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq2 CYCLE
           END-EXEC.
      *KO
      *DSNH846I DSNHSM5S LINE 83 COL 38  INVALID SPECIFICATION OF AN
      * IDENTITY COLUMN OR SEQUENCE OBJECT SEQUENCE ORG_SEQ2.
      * REASON CODE = 4
      *DSNH336I DSNHSM5S THE SCALE OF THE DECIMAL NUMBER MUST BE ZERO
           EXEC SQL ALTER SEQUENCE org_seq2 CACHE 100,1
           END-EXEC.
      *KO
      *The cache size should be positive
      *DSNH104I DSNHSM5S ILLEGAL SYMBOL"-". SOME SYMBOLS THAT MIGHT BE
      *LEGAL ARE: +<INTEGER>
           EXEC SQL ALTER SEQUENCE org_seq2 CACHE -20
           END-EXEC.
      *KO
      *The cache size should be greater than 2
      *DSNH846I DSNHSM5S  INVALID SPECIFICATION OF AN IDENTITY COLUMN
      * OR SEQUENCE OBJECT SEQUENCE ORG_SEQ2.
      * REASON CODE = 4
           EXEC SQL ALTER SEQUENCE org_seq2 CACHE 1
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq2 NO CACHE
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq2 ORDER
           END-EXEC.
           EXEC SQL ALTER SEQUENCE org_seq2 NO ORDER
           END-EXEC.
      *Combination
           EXEC SQL ALTER SEQUENCE org_seq2 NO ORDER RESTART
              INCREMENT BY 5
           END-EXEC.
      *KO
      *The maxValue must be greater than or equal to the minimum value
           EXEC SQL ALTER SEQUENCE org_seq2 MINVALUE 6 MAXVALUE 5
           END-EXEC.
      *KO
      *At least one option among RESTART, INCREMENT, MINVALUE, MAXVALUE,
      * CYCLE, CACHE or ORDER must be specified
      *DSNH104I DSNHPARS ILLEGAL
      * SYMBOL"<END-OF-STATEMEN". SOME SYMBOLS THAT MIGHT BE LEGAL ARE:
      *NOMAXVALUE NOMINVALUE NOORDER CYCLE NOCYCLE NOCACHE
           EXEC SQL ALTER SEQUENCE org_seq3
           END-EXEC.
           goback.
       end program DVZZMFT3.