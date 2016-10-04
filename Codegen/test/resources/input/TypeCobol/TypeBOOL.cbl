﻿       IDENTIFICATION DIVISION.
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



       PROCEDURE DIVISION.

       TRAITEMENT.
           SET Identifier  TO TRUE
           SET Identifier  TO FALSE
      * OK
           MOVE TRUE         TO Identifier
           MOVE FALSE        TO Identifier
           MOVE AnotherOne   TO Identifier
           MOVE Identifier   TO x
      * KO: a boolean can only receive booleans, TRUE or FALSE
           MOVE x   TO Identifier
           MOVE x   TO b OF a IN AGroup
           MOVE x   TO d      IN AGroup
      * KO: a boolean subordinates are read-only
           MOVE x   TO Identifier-value
           MOVE x   TO Identifier-false OF Identifier-value
      * OK
           MOVE x   TO c OF a IN AGroup
           MOVE Identifier    TO d      IN AGroup
           MOVE Identifier    TO b OF a IN AGroup
           MOVE d IN AGroup   TO b OF a IN AGroup
      * OK: copying chunks of memories of different layouts is standard COBOL practice
           MOVE x   TO      AGroup
           MOVE x   TO a IN AGroup
           .

       END PROGRAM Booleans.
