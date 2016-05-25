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
