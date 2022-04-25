*   //////////////////
*  // WITHOUT ELSE //
* //////////////////
IF condition THEN DISPLAY "OK" END-IF.
* no THEN
IF condition DISPLAY "OK" END-IF.
IF x OF y DISPLAY "OK" END-IF.
* no END-IF
IF condition THEN DISPLAY "OK".
* neither THEN, nor END-IF
IF condition DISPLAY "OK".

*   ///////////////
*  // WITH ELSE //
* ///////////////
IF condition THEN DISPLAY "OK" ELSE DISPLAY "KO" END-IF.
* no THEN
IF condition DISPLAY "OK" ELSE DISPLAY "KO" END-IF.
IF condition 
  DISPLAY "ONE" 
  DISPLAY "TWO" 
  DISPLAY "THREE" 
* a comment, just because
ELSE 
  DISPLAY "KO" 
END-IF.
* no END-IF
IF condition THEN DISPLAY "OK" ELSE DISPLAY "KO".
* neither THEN, nor END-IF
IF condition DISPLAY "OK" ELSE DISPLAY "KO".

*    //////////////////
*   //   NESTED     //
*  // WITHOUT ELSE //
* //////////////////
IF condition1 THEN IF condition2 THEN DISPLAY "OK" END-IF END-IF.
IF condition1 THEN
    IF condition2 THEN
        DISPLAY "OK"
    END-IF
END-IF.
* no THEN
IF condition1 IF condition2 THEN DISPLAY "OK" END-IF END-IF.
IF condition1 THEN IF condition2 DISPLAY "OK" END-IF END-IF.
IF condition1 IF condition2 DISPLAY "OK" END-IF END-IF.
IF condition1
  IF condition2
    DISPLAY "2"
  END-IF
  IF condition3
    DISPLAY "3"
  END-IF
END-IF.
IF condition1 IF condition2 IF condition3 IF condition4 IF condition5 IF condition6 IF condition7 IF condition8 IF condition9 DISPLAY "OK" END-IF END-IF END-IF END-IF END-IF END-IF END-IF END-IF END-IF.
IF condition1 
  IF condition2 
    IF condition3 
      IF condition4 
        IF condition5 
          IF condition6 
            IF condition7 
              IF condition8 
                IF condition9 
                  DISPLAY "OK"
                END-IF
              END-IF
            END-IF
          END-IF
        END-IF
      END-IF
    END-IF
  END-IF
END-IF.
* neither THEN, nor END-IF
IF condition1 IF condition2 DISPLAY "OK".
IF condition1 
  IF condition2 
    DISPLAY "OK".
IF condition1 IF condition2 IF condition3 IF condition4 IF condition5 IF condition6 IF condition7 IF condition8 IF condition9 DISPLAY "OK".
IF condition1 
  IF condition2 
    IF condition3 
      IF condition4 
        IF condition5 
          IF condition6 
            IF condition7 
              IF condition8 
                IF condition9 
                  DISPLAY "OK".

*    ///////////////
*   //  NESTED   //
*  // WITH ELSE //
* ///////////////
IF condition1
  IF condition2
    DISPLAY "121"
  ELSE
    DISPLAY "122"
  END-IF
  IF condition3
    DISPLAY "131"
  ELSE
    DISPLAY "132"
  END-IF
ELSE
  IF condition4
    DISPLAY "241"
  ELSE
    DISPLAY "242"
  END-IF
  IF condition5
    DISPLAY "251"
  ELSE
    DISPLAY "252"
  END-IF
END-IF.
IF condition11 
  IF condition21 
    IF condition31 
      IF condition41 
        IF condition51 
          IF condition61 
            IF condition71 
              IF condition81 
                IF condition91 
                  DISPLAY "OK"
                ELSE
                IF condition92
                  DISPLAY "KO"
                END-IF
              ELSE
              IF condition82
                DISPLAY "KO"
              END-IF
            ELSE
            IF condition72
              DISPLAY "KO"
            END-IF
          ELSE
          IF condition62
            DISPLAY "KO"
          END-IF
        ELSE
        IF condition52
          DISPLAY "KO"
        END-IF
      ELSE
      IF condition42
        DISPLAY "KO"
      END-IF
    ELSE
    IF condition32
      DISPLAY "KO"
    END-IF
  ELSE
  IF condition22
    DISPLAY "KO"
  END-IF
ELSE
IF condition12
  DISPLAY "KO"
END-IF.
IF condition1 DISPLAY "OK" ELSE DISPLAY "KO".
IF condition1 DISPLAY "1" ELSE IF condition2 DISPLAY "2" ELSE IF condition3 DISPLAY "3" ELSE DISPLAY "KO".

*IF x                   // OK
*IF NOT x               // OK
*IF (x)                 // OK
*IF x y                 // OK
*IF x (y) NOT = a (b)   // OK
*IF x AND y             // OK
*IF x (y) AND a         // KO, should be OK
*IF a AND x (y)         // KO, should be OK
*IF x (y)               // KO, should be OK I guess?
*IF x0 < y0 OR x1 > y1 OR x2 < y2 AND x3 > y3 OR x4 < y4 OR x5 > y5 AND x6 < y6 OR x7 > y7
*IF ( x1(y1) NOT = x2(y2)) AND NOT x3 (y3) THEN
IF ( x1 NOT = x2) AND NOT x3 THEN
  DISPLAY "KO"
END-IF.
IF (x  =  1) DISPLAY "RELATION IDENTIFIER VS NUMBER" END-IF.
IF (x  =  '1') DISPLAY "RELATION IDENTIFIER VS STRING" END-IF.
IF (x < 1 OR 2 AND 3 AND > 4) DISPLAY "ABBREVIATED RELATION" END-IF.
IF (x < 1 OR 2 AND 3 AND > 4 OR 5) DISPLAY "ABBREVIATED RELATION" END-IF.
IF (x < 1 OR 2 AND 3 OR 4 AND 5 OR 6) DISPLAY "ABBREVIATED RELATION" END-IF.
*IF ((x  =  1 OR 2)) DISPLAY "PARENTHESIS" END-IF.
*IF (x  =  (1 OR 2)) DISPLAY "PARENTHESIS" END-IF.

*IF NOT (ADPCTRE-CODPRD  =  ('1H' OR 'PH')) AND 
*   NOT ( (ADPCTRE-TYPPRSSCR = 'M') AND ( ADPCTRE-CODPRD = ('DX' OR 'DZ' OR 'FD' OR 'FK' OR 'FL' OR 'FO' OR 'FP' OR 'FV' OR '1B' OR '1F' OR '1K')) )
*IF (x = 1 OR 2 AND 3 AND 4 OR 5 OR 6 AND 7 OR 8 OR 9) DISPLAY "ABBREVIATED RELATION" END-IF.
*IF (x  =  (1 OR 2)) DISPLAY "RELATION IDENTIFIER VS BOOL" END-IF.
*IF NOT (x  =  ('1' OR '2'))
*  DISPLAY "OK"
*END-IF.

IF ( x ( y ) = 'OK'  ) CONTINUE END-IF.
IF ( a OF b ( x ) = 'OK'  ) CONTINUE END-IF.
IF ( a ( x OF y ) = 'OK'  ) THEN CONTINUE END-IF.
IF ( a OF b ( x OF y ) = 'OK'  ) THEN CONTINUE END-IF.

SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
IF x > 100,00 CONTINUE END-IF.
IF mytable(1, x) > 42 CONTINUE END-IF.

* With optional parentheses around identifiers
* class condition
if (Var1) is numeric display "numeric" end-if.
* condition-name or switch status condition
if (test1) display "test1" end-if.
* relation condition
if (var1) is greater than 1 display "> 1" end-if.
* sign condition
if (Var1) is positive display "positive" end-if.
