       IDENTIFICATION DIVISION.
       PROGRAM-ID. SampleGotos.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I PIC 9(4).
       01 J PIC 9(4).
       PROCEDURE DIVISION.
       L1.
           MOVE 2 TO I
           MOVE I TO J
           ADD 1 TO J.
       L2.
           MOVE 1 TO I
           IF J = 999 THEN
                GO TO L4
           END-IF.
       L3.
           ADD 1 TO J
           IF J = 999 THEN
                GO TO L4
           END-IF
           SUBTRACT 4 FROM J.
       L4.
           GO TO L2.
       END PROGRAM SampleGotos.
      