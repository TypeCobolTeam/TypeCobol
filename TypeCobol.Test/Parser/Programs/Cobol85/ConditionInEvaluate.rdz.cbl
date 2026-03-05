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

       PROCEDURE DIVISION.
           EVALUATE COND1
                WHEN TRUE
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
                WHEN "A" THRU "B"
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE "A"
                WHEN VAR1
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE TRUE
                WHEN COND1
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1 = "A"
                WHEN TRUE
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE COND1 AND COND2
                WHEN TRUE
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-INDEX1 + VAR-INDEX2
                WHEN 1
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1 IS NUMERIC
               WHEN TRUE
                  DISPLAY "OK"
               WHEN OTHER
                  DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-INDEX1 IS GREATER THAN 1
               WHEN TRUE
                  DISPLAY "OK"
               WHEN OTHER
                  DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-INDEX1 IS POSITIVE
               WHEN TRUE
                  DISPLAY "OK"
               WHEN OTHER
                  DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE SPACES
               WHEN VAR1
                  DISPLAY "OK"
               WHEN OTHER
                  DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE COND1 ALSO COND2
                WHEN TRUE ALSO TRUE
                    DISPLAY "OK"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE COND1 AND VAR2
                WHEN TRUE
                    DISPLAY "KO: EVALUATE NOT VALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE COND1 ALSO VAR1 AND COND2 AND VAR2
                WHEN TRUE ALSO TRUE
                    DISPLAY "KO: EVALUATE NOT VALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1 AND VAR2 = "B"
                WHEN TRUE
                    DISPLAY "KO: EVALUATE NOT VALID"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE NOT-REFERENCED
               WHEN TRUE
                   DISPLAY 'KO: EVALUATE NOT REFERENCED'
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

           GOBACK
           .

       END PROGRAM CHKCNDTN.