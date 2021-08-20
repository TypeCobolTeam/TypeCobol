       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 item   PIC X(20).
       01 part   PIC X(20).
       01 var0   PIC 9 VALUE 0.
       01 var1   PIC 9 VALUE 1.
      
       PROCEDURE DIVISION.
      * OK this is a shorthand
           MOVE item(2:) TO part
      * KO zero position
           MOVE item(0:) TO part
      * KO negative position
           MOVE item(-1:) TO part
      * KO by a previous step
           MOVE item(:2) TO part
      
      * KO zero position
           MOVE item(0:5) TO part
      * KO zero length
           MOVE item(1:0) TO part
      * KO*2 zero values
           MOVE item(0:0) TO part
      * KO negative position
           MOVE item(-1:5) TO part
      * KO negative length
           MOVE item(1:-5) TO part
      * KO*2 negative values
           MOVE item(-1:-5) TO part
      * OK
           MOVE item(1.32:5) TO part
      * OK
           MOVE item(1:5.84) TO part
      * OK
           MOVE item(1.32:5.84) TO part
      * KO zero position
           MOVE item(0.0:5) TO part
      * KO zero length
           MOVE item(1:0.0) TO part
      * KO*2 zero values
           MOVE item(0.0:0.0) TO part
      * KO negative position
           MOVE item(-1.32:5) TO part
      * KO negative length
           MOVE item(1:-5.84) TO part
      * KO*2 negative values
           MOVE item(-1.32:-5.84) TO part
      
      * OK Variables are not checked
           MOVE item(var0:) TO part
           MOVE item(var1:) TO part
           MOVE item(2:var0) TO part
           MOVE item(2:var1) TO part
      
      * OK Expressions are not checked
           MOVE item(2 * 3  : 4     ) TO part
           MOVE item(2      : 4 * 2 ) TO part
           MOVE item(2 * 2  : 4 * 2 ) TO part
           MOVE item(-2 + 1 : 2     ) TO part
           MOVE item(2      : 4 * -2) TO part
           .
       END PROGRAM Pgm.