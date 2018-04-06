       IDENTIFICATION DIVISION.
       PROGRAM-ID. Callee.

       PROCEDURE DIVISION.

       DECLAR...OOOHHhh that's an error..
         .
         PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       DECLARE PROCEDURE MySecondPublicProcedure PUBLIC
           INPUT vect2        TYPE Vect2
         .
         PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.
       END PROGRAM Callee.
