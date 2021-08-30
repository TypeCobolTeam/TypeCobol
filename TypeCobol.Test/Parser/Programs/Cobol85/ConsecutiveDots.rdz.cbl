       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *OK
       01 A pic X.
      *KO Invalid Picture
       01 B pic X..
      *KO Unexpected '.'
       01 C pic X. .
      *KO not reported because of previous error
       01 var1..
          05 var2 pic XX.
      *KO Invalid Picture
          05 var3 pic X.05 var4 pic X.
       PROCEDURE DIVISION..
           move A to B..
           move A to B...move A to B.
           goback
           .
       END PROGRAM Pgm.