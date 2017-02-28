       IDENTIFICATION DIVISION.
       PROGRAM-ID. Booleans.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  x PIC X.
      * Booleans declaration
       01  Identifier TYPE BOOL.
       01  AnotherOne TYPE BOOL.
       01  AGroup.
           05  a.
             10  c PIC X.
             10  b TYPE BOOL.
           05  d TYPE BOOL.

000000 01 group.
000000    05 array occurs 5.
000000       10 checkToDo type bool.

       PROCEDURE DIVISION.

       TRAITEMENT.
           SET Identifier  TO TRUE
           SET Identifier  TO FALSE
      * OK
           MOVE TRUE         TO Identifier
           MOVE FALSE        TO Identifier
           MOVE AnotherOne   TO Identifier
           MOVE Identifier   TO x
      * OK
           MOVE x   TO c OF a IN AGroup
           MOVE Identifier    TO d      IN AGroup
           MOVE Identifier    TO b OF a IN AGroup
           MOVE d IN AGroup   TO b OF a IN AGroup
      * OK: copying chunks of memories of different layouts is standard COBOL practice
           MOVE x   TO      AGroup
           MOVE x   TO a IN AGroup
      * OK: with occurs
           set checkToDo(1) to true
           set checkToDo(1) to false
           .

       END PROGRAM Booleans.
