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
       01 Var-txt2             PIC X.
       01 Var-DBCS             PIC G DISPLAY-1.
       01  COLORS              PIC  X(9).
         88  BLUE          VALUE 'BLUE'.
         88  GREEN         VALUE 'GREEN'.
         88  TURQUOISE     VALUE 'BLUE' 'GREEN'.
       PROCEDURE DIVISION.
      * RelationalOperator
      * 0 and ConditionOperands and NumericVariableOperands
           IF X = Z
             CONTINUE
           END-IF
      * 1 and NumericVariableOperand
           IF X equal Y
             CONTINUE
           END-IF
      * 2
           IF X not = Z
             CONTINUE
           END-IF
      * 3
           IF X not equal Y
             CONTINUE
           END-IF
      * 4
           IF X < Z
             CONTINUE
           END-IF
      * 5
           IF X greater than or equal to Y
             CONTINUE
           END-IF
      * 6
           IF X not less than Y
             CONTINUE
           END-IF
      
      * LogicalOperators
      * 0
           IF X = Z AND X = Y
             CONTINUE
           END-IF.
      * 1
           IF X = Y AND Z = Y
             CONTINUE
           END-IF.
      * 2 
           IF X = Z OR X = Y
             CONTINUE
           END-IF.
      * 3 
           IF NOT (X = Z)
             CONTINUE
           END-IF.
      
      * ArithmeticOperator
      * 0
           COMPUTE X = Y / Z
      * 1
           COMPUTE Z = X / Y
      * 2
           COMPUTE X = Y + Z
      * 3
           COMPUTE X = - Y

      
      * Sign condition
      * 0
           IF X IS ZERO
             CONTINUE
           END-IF.
      * 1
           IF Y IS ZERO
             CONTINUE
           END-IF.
      * 2
           IF X IS NOT ZERO
             CONTINUE
           END-IF.
      * 3
           IF X IS NOT ZERO
             CONTINUE
           END-IF.
      * 4
           IF X IS POSITIVE
             CONTINUE
           END-IF.
      * 5
           IF X IS NEGATIVE
             CONTINUE
           END-IF.           
      
      * Class condition
      * 0
           IF Var-txt2 IS NUMERIC
             CONTINUE
           END-IF.
      * 1
           IF Var-txt IS NUMERIC
             CONTINUE
           END-IF.
      * 2
           IF Var-txt IS NOT NUMERIC
             CONTINUE
           END-IF.
      * 3
           IF Var-txt IS ALPHABETIC
             CONTINUE
           END-IF.
      * 4
           IF Var-txt IS ALPHABETIC-LOWER
             CONTINUE
           END-IF.
      * 5
           IF Var-txt IS ALPHABETIC-UPPER
             CONTINUE
           END-IF.
      * 6
           IF Var-txt IS MYCLASS
             CONTINUE
           END-IF.
      
      * Condition-name condition
      * 0
           IF GREEN
             CONTINUE
           END-IF.
      * 1     
           IF GREEN
             CONTINUE
           END-IF.
      * 2     
           IF BLUE
             CONTINUE
           END-IF.
      
      *     Switch-status condition
      * 3
           IF MYSWITCH-ON
             CONTINUE
           END-IF.
      * 4
           IF MYSWITCH-ON
             CONTINUE
           END-IF.      

      * Other expressions
      *     Abbreviated expression
      * 0
           IF X > 3 OR 4
             CONTINUE
           END-IF
      * 1
           IF Y < 6 OR 12
             CONTINUE
           END-IF
      
           GOBACK
           .
      
       END PROGRAM Pgm.