      * 7 CodeElements errors
      * "1"@(17:12>17:36): [27:1] Syntax error : Function POW is missing parameter 2 of type Numeric
      * "1"@(18:12>18:40): [27:1] Syntax error : Function POW only takes 2 parameters
      * "1"@(20:12>20:38): [27:1] Syntax error : Symbol i is not referenced
      * "1"@(20:12>20:38): [27:1] Syntax error : Symbol j is not referenced
      * "1"@(22:12>22:38): [27:1] Syntax error : Function POW expected parameter 2 of type Numeric (actual: BOOL)
      * "1"@(24:12>24:38): [27:1] Syntax error : Function POW expected parameter 2 of max length 3 (actual: 5)
      * "1"@(26:12>26:41): [27:1] Syntax error : Symbol POWAAA is not referenced
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Functions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  x PIC 9.
       01  y PIC 9(3).
       01  z PIC 9(5).
       01  b TYPE BOOL.

       PROCEDURE DIVISION.

       TRAITEMENT.
           SET x TO FUNCTION POW (x y)
      * KO: wrong number of parameters
           SET x TO FUNCTION POW (y)
           SET x TO FUNCTION POW (x y z)
      * KO: undefined parameters
           SET x TO FUNCTION POW (i j)
      * KO: 2nd parameter of wrong type
           SET x TO FUNCTION POW (x b)
      * KO: 2nd parameter too-large
           SET x TO FUNCTION POW (z z)
      * KO: function undeclared
           SET x TO FUNCTION POWAAA (x y)
           .

       END PROGRAM Functions.
