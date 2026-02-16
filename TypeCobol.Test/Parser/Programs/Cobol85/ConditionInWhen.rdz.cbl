      * Commentaire
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHKCNDTN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MAIN-GROUP.
          05 VAR1      PIC X.
             88 COND1        VALUE 'A'.
             88 COND2        VALUE 'B'.
          05 VAR2      PIC X.
       77 VAR-INDEX1   PIC 9(4) COMP-5.
       77 VAR-INDEX2   PIC 9(4) COMP-5.
       77 VAR-ALPHA        PIC X.
       77 VAR-ALPHA-EDITED PIC XB.
       77 VAR-ALPHABETIC   PIC A.
       77 VAR-NAT          PIC N.
       77 VAR-NAT-EDITED   PIC NB.
       77 VAR-NUM          PIC 9.
       77 VAR-NUM-EDITED   PIC 9B.

       PROCEDURE DIVISION.
           EVALUATE COND1
                WHEN TRUE
                    DISPLAY "OK"
                WHEN COND2
                    DISPLAY "OK"
                WHEN VAR1 = "A"
                    DISPLAY "OK"
                WHEN VAR1 IS NUMERIC
                    DISPLAY "OK"
                WHEN VAR-INDEX1 IS GREATER THAN 1
                    DISPLAY "OK"
                WHEN VAR-INDEX1 IS POSITIVE
                    DISPLAY "OK"
                WHEN ANY
                    DISPLAY "ANY"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
                WHEN "A" THRU "B"
                    DISPLAY "OK"
                WHEN "A" THRU VAR2
                    DISPLAY "OK"
                WHEN NOT 1 THROUGH 9
                    DISPLAY "OK"
                WHEN NOT "A"
                    DISPLAY "OK"
                WHEN ANY
                    DISPLAY "ANY"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE "A"
                WHEN VAR1 THRU VAR2
                    DISPLAY "OK"
                WHEN NOT VAR1
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE 1
                WHEN VAR-INDEX1 + VAR-INDEX2
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE TRUE
                WHEN VAR1 AND COND2
                    DISPLAY "KO: WHEN NOT VALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE TRUE
                WHEN COND1 THRU COND2
                    DISPLAY "KO: THRU INVALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
                WHEN COND1 THRU "B"
                    DISPLAY "KO: THRU INVALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
                WHEN COND1 THRU VAR2
                    DISPLAY "KO: THRU INVALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE TRUE
                WHEN TRUE THRU VAR2
                    DISPLAY "KO: THRU NOT PARSED"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
      * 2 errors :
      *   1st error = THRU is not parsed
      *   So our parser considers only condition VAR2 = "A"
      *   which is not compliant with VAR1 (=> 2nd error)
                WHEN VAR2 = "A" THRU VAR2
                    DISPLAY "KO: THRU INVALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
                WHEN 1 THROUGH COND1
                    DISPLAY "KO: THRU INVALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
                WHEN "A" THRU 9
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN 1 THRU "Z"
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR2 THRU 9
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN 1 THRU VAR2
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-INDEX1 THRU VAR2
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR2 THRU VAR-INDEX2
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
                WHEN VAR-ALPHA THRU VAR-ALPHA-EDITED
                    DISPLAY "OK"
                WHEN VAR-ALPHA THRU VAR-ALPHABETIC
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-ALPHA THRU VAR-NAT
                    DISPLAY "OK"
                WHEN VAR-ALPHA THRU VAR-NAT-EDITED
                    DISPLAY "OK"
                WHEN VAR-ALPHA THRU VAR-NUM
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-ALPHA THRU VAR-NUM-EDITED
                    DISPLAY "OK"
                WHEN VAR-ALPHA-EDITED THRU VAR-ALPHABETIC
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-ALPHA-EDITED THRU VAR-NAT
                    DISPLAY "OK"
                WHEN VAR-ALPHA-EDITED THRU VAR-NAT-EDITED
                    DISPLAY "OK"
                WHEN VAR-ALPHA-EDITED THRU VAR-NUM
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-ALPHA-EDITED THRU VAR-NUM-EDITED
                    DISPLAY "OK"
                WHEN VAR-ALPHABETIC THRU VAR-NAT
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-ALPHABETIC THRU VAR-NAT-EDITED
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-ALPHABETIC THRU VAR-NUM
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-ALPHABETIC THRU VAR-NUM-EDITED
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-NAT THRU VAR-NAT-EDITED
                    DISPLAY "OK"
                WHEN VAR-NAT THRU VAR-NUM
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-NAT THRU VAR-NUM-EDITED
                    DISPLAY "OK"
                WHEN VAR-NAT-EDITED THRU VAR-NUM
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN VAR-NAT-EDITED THRU VAR-NUM-EDITED
                    DISPLAY "OK"
                WHEN VAR-NUM THRU VAR-NUM-EDITED
                    DISPLAY "KO: DIFFERENT TYPES IN THRU"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE TRUE
                WHEN (VAR2 = "B") AND VAR1
                    DISPLAY "KO: WHEN NOT VALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE COND1
                WHEN NOT-REFERENCED
                   DISPLAY 'KO: WHEN NOT REFERENCED'
                WHEN 
                    DISPLAY "KO: MISSING WHEN"
                WHEN TRUE
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           GOBACK
           .

       END PROGRAM CHKCNDTN.