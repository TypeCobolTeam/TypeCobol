      * Commentaire
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHKCNDTN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           UPSI-0 IS VAR-SWITCH
             ON STATUS IS VAR-SWITCH-ON
             OFF STATUS IS VAR-SWITCH-OFF.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MAIN-GROUP.
          05 VAR1      PIC X.
             88 COND1        VALUE 'A'.
             88 COND2        VALUE 'B'.
          05 VAR2      PIC X.
       PROCEDURE DIVISION.
           IF (COND1) THEN
              DISPLAY "OK: Condition-name with level 88"
           END-IF
           IF (VAR1 = 'A') THEN
              DISPLAY "OK: Relation condition"
           END-IF
           IF (COND1) AND (VAR1 = 'A') AND (VAR2 = 'B') THEN
              DISPLAY "OK: valid Complex condition"
           END-IF
      * Should be OK but is KO: we do not handle UPSI switch (see #2355)
           IF (VAR-SWITCH-ON)
              DISPLAY "Switch-status condition theorically OK"
           END-IF.
      * IBM reports 2 errors but we only report one = not referenced
           IF (VAR-NOT-FOUND) THEN
              DISPLAY "KO: data not referenced"
           END-IF
           IF (MAIN-GROUP) THEN
              DISPLAY "KO: Condition-name with group"
           END-IF
           IF (VAR2) THEN
              DISPLAY "KO: Condition-name with data"
           END-IF
           IF (VAR1) THEN
              DISPLAY "KO: Condition-name with data instead of level 88"
           END-IF
           IF (VAR2 AND COND1) THEN
              DISPLAY "KO: invalid Complex condition"
           END-IF
           IF (COND1) AND (VAR1 = 'A') AND (VAR2) THEN
              DISPLAY "KO: invalid Complex condition"
           END-IF
           GOBACK
           .

       END PROGRAM CHKCNDTN.