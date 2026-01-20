      * Commentaire
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHKCDNNM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MAIN-GROUP.
          05 VAR1      PIC X.
             88 COND1        VALUE 'A'.
             88 COND2        VALUE 'B'.
          05 VAR2 REDEFINES VAR1 PIC 9.
          05 VAR3      PIC X.
          66 VAR4 RENAMES VAR3.
       77 VAR-INDEX1   PIC 9(4) COMP-5.
       77 VAR-INDEX2   PIC 9(4) COMP-5.
       77 VAR5         PIC A.

       PROCEDURE DIVISION.

      * ***************
      * PERFORM VARYING
      * ***************

      * KO: Condition-name with data
           PERFORM PAR1
           VARYING VAR-INDEX1 FROM 1 BY 1 UNTIL VAR1
           AFTER VAR-INDEX2 FROM 1 BY 1
           UNTIL (COND1) AND (VAR1 = 'A') AND (VAR3)

      * KO: Condition-name with redefines
           PERFORM PAR1
           VARYING VAR-INDEX1 FROM 1 BY 1 UNTIL VAR2
           AFTER VAR-INDEX2 FROM 1 BY 1
           UNTIL (COND1) AND (VAR1 = 'A') AND (VAR2)

      * KO: Condition-name with renames
           PERFORM PAR1
           VARYING VAR-INDEX1 FROM 1 BY 1 UNTIL VAR4
           AFTER VAR-INDEX2 FROM 1 BY 1
           UNTIL (COND1) AND (VAR4) AND (VAR1 = 'A')

      * KO: Condition-name with data 77
           PERFORM PAR1
           VARYING VAR-INDEX1 FROM 1 BY 1
           UNTIL (VAR5) OR (VAR4 = 'A') OR COND2
           AFTER VAR-INDEX2 FROM 1 BY 1
           UNTIL VAR5

      * KO: data not referenced
           PERFORM PAR1
           VARYING VAR-INDEX1 FROM 1 BY 1 UNTIL COND2
           AFTER VAR-INDEX2 FROM 1 BY 1
           UNTIL VAR-NOT-DEFINED

      * OK
           PERFORM PAR1
           VARYING VAR-INDEX1 FROM 1 BY 1 UNTIL COND1
           AFTER VAR-INDEX2 FROM 1 BY 1
           UNTIL (COND1) AND (VAR3 = 'A') AND (COND2)

           
      * ***************
      * PERFORM SIMPLE
      * ***************

      * KO: Condition-name with data
           PERFORM PAR1
           UNTIL (COND1)
      *        AND COND2   
               AND (VAR1) AND (VAR5 = 'A')

      * OK
           PERFORM PAR1
           UNTIL (COND1)
      *        AND VAR2   
               AND (VAR1 = 'A') AND (COND2)	

           GOBACK
           .

       PAR1.
           DISPLAY 'in PAR1'
           .

       END PROGRAM CHKCDNNM.