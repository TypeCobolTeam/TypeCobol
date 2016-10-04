       IDENTIFICATION DIVISION.
       PROGRAM-ID. Functions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  x PIC 9.
       01  y PIC 9(3).
       01  z PIC 9(5).
       01  b TYPE BOOL.

       PROCEDURE DIVISION.
       
       DECLARE function POW PRIVATE
             INPUT x PIC 9(05)
                   y PIC 9(03)
             RETURNING result PIC 9(08)
         .
         PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       TRAITEMENT.
           MOVE FUNCTION POW (x y)    TO x
      * KO: wrong number of parameters
           MOVE FUNCTION POW ()       TO x
           MOVE FUNCTION POW (y)      TO x
           MOVE FUNCTION POW (x y z)  TO x
      * KO: undefined parameters
           MOVE FUNCTION POW (i j)    TO x
      * KO: 2nd parameter of wrong type
           MOVE FUNCTION POW (x b)    TO x
      * KO: 2nd parameter too-large
           MOVE FUNCTION POW (z z)    TO x
      * KO: function undeclared
           MOVE FUNCTION POWAAA (x y) TO x
           .

       END PROGRAM Functions.
