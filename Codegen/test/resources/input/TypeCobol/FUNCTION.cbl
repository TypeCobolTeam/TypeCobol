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