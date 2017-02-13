       IDENTIFICATION DIVISION.
       PROGRAM-ID. IndexTester.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ZONE PIC X(5).
       01  TB-1.
           05  EL-1 PIC X(5) OCCURS 10 INDEXED BY IND-1.

       PROCEDURE DIVISION.

           MOVE EL-1(IND-1) TO ZONE  
           SET IND-1 TO 1
           .  

       END PROGRAM IndexTester.