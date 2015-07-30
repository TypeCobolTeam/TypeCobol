*   //////////////////
*  // WITHOUT ELSE //
* //////////////////
IF condition THEN DISPLAY "OK" END-IF.
* no THEN
IF condition DISPLAY "OK" END-IF.
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