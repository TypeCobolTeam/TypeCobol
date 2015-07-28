
*   //////////////
*  // FORMAT 3 //
* //////////////
* PERFORM procedurename UNTIL conditionname.

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