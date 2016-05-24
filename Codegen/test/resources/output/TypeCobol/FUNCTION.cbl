      * 1 CodeElements errors
      * "1"@(17:12>17:36): [27:1] Syntax error : Function POW is missing parameter 2 of type Numeric
      * "1"@(18:12>18:40): [27:1] Syntax error : Function POW only takes 2 parameters
      * "1"@(20:12>20:38): [27:1] Syntax error : Function POW expected parameter 2 of type Numeric (actual: BOOL)
      * "1"@(22:12>22:41): [27:1] Syntax error : Function POWAAA is not referenced
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Functions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  x PIC 9.
       01  y PIC 9.
       01  z PIC 9.
       01  b TYPE BOOL.

       PROCEDURE DIVISION.

       TRAITEMENT.
           SET x TO FUNCTION POW (x y)
      * KO: wrong number of parameters
           SET x TO FUNCTION POW (y)
           SET x TO FUNCTION POW (x y z)
      * KO: wrong 2nd parameter type
           SET x TO FUNCTION POW (x b)
      * KO: function undeclared
           SET x TO FUNCTION POWAAA (x y)
           .

       END PROGRAM Functions.