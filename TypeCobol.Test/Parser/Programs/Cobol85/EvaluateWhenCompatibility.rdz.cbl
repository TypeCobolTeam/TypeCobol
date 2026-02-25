       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVALWHEN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MAIN-GROUP.
          05 VAR1      PIC X.
             88 COND1        VALUE 'A'.
             88 COND2        VALUE 'B'.
          05 VAR2 REDEFINES VAR1 PIC X.
          05 VAR3      PIC X.
          66 VAR4 RENAMES VAR3.
       77 VAR-INDEX1   PIC 9(4) COMP-5.
       77 VAR-INDEX2   PIC 9(4) COMP-5.
       77 VAR5         PIC A.
       77 VAR6         PIC 9.
       77 VAR7         PIC 9.

       PROCEDURE DIVISION.
	  
           EVALUATE COND1
                WHEN "A" THRU "B"
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN VAR1 THRU VAR2
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN 1 THRU 9
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN VAR6 THRU VAR7
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN COND1
                    DISPLAY "OK"
                WHEN TRUE
                    DISPLAY "OK"
                WHEN VAR1
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN NOT COND2
                    DISPLAY "OK"
                WHEN NOT VAR5
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN NOT "A"
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN ANY
                    DISPLAY "ANY"
                WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1
                WHEN "A" THRU "B"
                    DISPLAY "OK"
                WHEN VAR1 THRU VAR2
                    DISPLAY "OK"
                WHEN 1 THRU 9
                    DISPLAY "OK"
                WHEN VAR6 THRU VAR7
                    DISPLAY "OK"
                WHEN COND1
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN TRUE
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN VAR1
                    DISPLAY "OK"
                WHEN NOT COND2
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN NOT VAR5
                    DISPLAY "OK"
                WHEN NOT "A"
                    DISPLAY "OK"
                WHEN ANY
                    DISPLAY "ANY"
                WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE "A"
                WHEN "A" THRU "B"
                    DISPLAY "KO: Comparison between literals"
                WHEN VAR1 THRU VAR2
                    DISPLAY "OK"
                WHEN 1 THRU 9
                    DISPLAY "KO: Comparison between literals"
                WHEN VAR6 THRU VAR7
                    DISPLAY "OK"
                WHEN COND1
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN TRUE
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN VAR1
                    DISPLAY "OK"
                WHEN NOT COND2
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN NOT VAR5
                    DISPLAY "OK"
                WHEN NOT "A"
                    DISPLAY "KO: Comparison between literals"
                WHEN ANY
                    DISPLAY "ANY"
                WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE TRUE
                WHEN "A" THRU "B"
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN VAR1 THRU VAR2
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN 1 THRU 9
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN VAR6 THRU VAR7
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN COND1
                    DISPLAY "OK"
                WHEN TRUE
                    DISPLAY "OK"
                WHEN VAR1
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN NOT COND2
                    DISPLAY "OK"
                WHEN NOT VAR5
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN NOT "A"
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN ANY
                    DISPLAY "ANY"
                WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR1 = "A"
                WHEN TRUE
                    DISPLAY "OK"
                WHEN VAR1
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN NOT COND2
                    DISPLAY "OK"
                WHEN NOT VAR5
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN NOT "A"
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN ANY
                    DISPLAY "ANY"
                WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE
		   
           EVALUATE TRUE ALSO FALSE ALSO VAR1
                WHEN COND1 ALSO VAR2 ALSO "A"
                    DISPLAY "KO: WHEN not match type of EVALUATE"
                WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

            EVALUATE "A"
                WHEN "B"
                    DISPLAY "KO: Comparison between literals"
                WHEN 9
                    DISPLAY "KO: Comparison between literals"
                WHEN VAR1 THRU "Z"
                    DISPLAY "KO: Comparison between literals"
                WHEN OTHER
                    DISPLAY "OTHER"
            END-EVALUATE

            EVALUATE VAR1 ALSO 1
                WHEN "A" ALSO "B"
                    DISPLAY "KO: Comparison between literals"
                WHEN 1 ALSO 2 THRU 9
                    DISPLAY "KO: Comparison between literals"
                WHEN OTHER
                    DISPLAY "OTHER"
            END-EVALUATE

           EVALUATE COND1 ALSO COND2
                WHEN TRUE
                    DISPLAY "KO: nb of EVAL and WHEN differ"
                WHEN TRUE ALSO FALSE
                    DISPLAY "OK"
                WHEN TRUE ALSO FALSE ALSO TRUE
                    DISPLAY "KO: nb of EVAL and WHEN differ"
                WHEN ANY ALSO ANY
                    DISPLAY "ANY"
                WHEN OTHER
                     DISPLAY "OTHER"
           END-EVALUATE

      * KO: not referenced
           EVALUATE NOT-REFERENCED1
                WHEN NOT-REFERENCED2
                    DISPLAY 'KO: not referenced'
                WHEN COND1
                    DISPLAY "OK"
                WHEN TRUE
                    DISPLAY "OK"
                WHEN VAR1
                    DISPLAY "OK"
                WHEN "A"
                    DISPLAY "OK"
                WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE
		   
           GOBACK
           .
		   
       END PROGRAM EVALWHEN.