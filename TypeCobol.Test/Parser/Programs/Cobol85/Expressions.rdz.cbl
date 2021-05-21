       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Pgm.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           UPSI-0 IS MYSWITCH
             ON STATUS IS MYSWITCH-ON
             OFF STATUS IS MYSWITCH-OFF
           CLASS MYCLASS IS "G" THRU "O".
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X                    PIC 9.
       01 Y                    PIC 9.
       01 Z                    PIC 9.
       01 Var-txt              PIC X.
       01 Var-DBCS             PIC G DISPLAY-1.
       01  COLORS              PIC  X(9).
         88  BLUE          VALUE 'BLUE'.
         88  GREEN         VALUE 'GREEN'.
         88  TURQUOISE     VALUE 'BLUE' 'GREEN'.
       PROCEDURE DIVISION.
      * RelationalOperator
           IF X = Z
             CONTINUE
           END-IF
      
           IF X equal Z
             CONTINUE
           END-IF
      
           IF X not = Z
             CONTINUE
           END-IF
      
           IF X not equal Z
             CONTINUE
           END-IF
      
           IF X < Y
             CONTINUE
           END-IF
      
           IF X less than Y
             CONTINUE
           END-IF
      
           IF X > Y
             CONTINUE
           END-IF
      
           IF X greater than Y
             CONTINUE
           END-IF
      
           IF X <= Y
             CONTINUE
           END-IF
      
           IF X less than or equal to Y
             CONTINUE
           END-IF
      
           IF X not greater than Y
             CONTINUE
           END-IF
      
           IF X >= Y
             CONTINUE
           END-IF
      
           IF X greater than or equal to Y
             CONTINUE
           END-IF
      
           IF X not less than Y
             CONTINUE
           END-IF
      
      * LogicalOperators
           IF X = Z AND X = Y
             CONTINUE
           END-IF.
      
           IF X = Z OR X = Y
             CONTINUE
           END-IF.
      
           IF NOT (X = Z)
             CONTINUE
           END-IF.
      
      * ArithmeticOperator
           COMPUTE X = Y / Z
      
           COMPUTE X = Y - Z
      
           COMPUTE X = Y + Z
      
           COMPUTE X = Y * Z
      
           COMPUTE X = Y ** Z
      
           COMPUTE X = - Y

           COMPUTE X ROUNDED = Z

           ADD Y TO X ROUNDED
           SUBTRACT Y FROM X ROUNDED
           MULTIPLY Y BY Z GIVING X ROUNDED
           DIVIDE Y BY Z GIVING X ROUNDED
      
      * Sign condition
           IF X IS ZERO
             CONTINUE
           END-IF.
      
           IF X IS POSITIVE
             CONTINUE
           END-IF.
      
           IF X IS NEGATIVE
             CONTINUE
           END-IF.
      
           IF X IS NOT ZERO
             CONTINUE
           END-IF.
      
      * Class condition
           IF Var-txt IS NUMERIC
             CONTINUE
           END-IF.
      
           IF Var-txt IS ALPHABETIC
             CONTINUE
           END-IF.
      
           IF Var-txt IS ALPHABETIC-LOWER
             CONTINUE
           END-IF.
      
           IF Var-txt IS ALPHABETIC-UPPER
             CONTINUE
           END-IF.
      
           IF Var-DBCS IS DBCS
             CONTINUE
           END-IF.
      
           IF Var-DBCS IS KANJI
             CONTINUE
           END-IF.
      
           IF Var-txt IS MYCLASS
             CONTINUE
           END-IF.
      
           IF Var-txt IS NOT NUMERIC
             CONTINUE
           END-IF.
      
      * Condition-name condition
           IF GREEN
             CONTINUE
           END-IF.
      
      * Switch-status condition
           IF MYSWITCH-ON
             CONTINUE
           END-IF.
      
      * Big expression
           IF X > 3 OR 5 < X AND Y < 4 OR Y + X not = 3
            AND Y - 5 NOT LESS Z + 2 AND Y <= X
            AND X not = Y OR 9 > Y AND X / Z EQUAL 2 OR Y + Y > 50
            OR Y < X - 30
             CONTINUE
           END-IF
      
      
      * Abbreviated expression
           IF X > 3 OR 4
             CONTINUE
           END-IF
      
      * Multiline
           IF X > 3 AND 5 < Y
             OR 5 < Y AND X > 3
             CONTINUE
           END-IF
      
           IF X > 3
             OR 3
             CONTINUE
           END-IF
      
           IF X not = Y
            OR Y not = X
             CONTINUE
           END-IF
      
      * Multiline and spaced
           IF                 X > 3
             OR 3
             CONTINUE
           END-IF
      
           IF X > 3
                      OR 3
             CONTINUE
           END-IF
      
      * Not at begining and spaced
           IF X
             not = X
             CONTINUE
           END-IF
      
           IF          X
             not = X
             CONTINUE
           END-IF
      
           IF X
                  not = X
             CONTINUE
           END-IF
      
           GOBACK
           .
      
       END PROGRAM Pgm.