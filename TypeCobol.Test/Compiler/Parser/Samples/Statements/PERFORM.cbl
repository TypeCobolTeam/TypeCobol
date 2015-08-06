*   //////////////
*  // FORMAT 1 //
* //////////////
PERFORM procedurename.

*   //////////////
*  // FORMAT 1 //
* //////////////
PERFORM procedurename 10 TIMES.
PERFORM 10 TIMES
  DISPLAY "HELLO"
END-PERFORM.

*   //////////////
*  // FORMAT 3 //
* //////////////
PERFORM procedurename UNTIL conditionname.
PERFORM procedurename
PERFORM UNTIL conditionname
  PERFORM procedurename
  IF condition
    PERFORM procedurename
  END-IF
  PERFORM procedurename
END-PERFORM
PERFORM procedurename
.

*   //////////////
*  // FORMAT 4 //
* //////////////
* with simple condition
PERFORM procedurename VARYING x FROM 1 BY 1 UNTIL x NOT = ZERO. 
* with complex condition
PERFORM procedurename VARYING x FROM 1 BY 1 UNTIL
* Format 1
      x < 0 OR x NOT = ZERO
* Format 2, Format 3
   OR pointername1 EQUAL TO pointername2
.
PERFORM VARYING x FROM 1 BY 1 UNTIL condition CONTINUE END-PERFORM.
PERFORM VARYING x FROM 1 BY 1 UNTIL condition1 OR condition2 CONTINUE END-PERFORM.
PERFORM VARYING x FROM 1 BY 1 UNTIL condition (x) CONTINUE END-PERFORM.