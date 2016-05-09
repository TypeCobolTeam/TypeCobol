       IDENTIFICATION DIVISION.
       PROGRAM-ID. Booleans.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  BOOL            TYPEDEF STRONG.
         05  var         VALUE 'T'.
         05  var-value   VALUE 'F'.

       01  x PIC X.

      * Booleans declaration
       01  Identifier TYPE BOOL.
       01  AnotherOne TYPE BOOL.
      * WARNING: initialization of a group containing booleans
       01  AGroup.
         05  a PIC X.
           10  a PIC X.
           10  b TYPE BOOL.
         05  b TYPE BOOL.




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
      * KO: a boolean subordinates are read-only
           MOVE x   TO Identifier-value
           MOVE x   TO Identifier-false OF Identifier-value
      * OK
           MOVE x   TO a      IN AGroup
           MOVE x   TO a OF a IN AGroup
           MOVE Identifier    TO b      IN AGroup
           MOVE Identifier    TO b OF a IN AGroup
           MOVE b IN AGroup   TO b OF a IN AGroup
      * KO: moving to a group containing booleans
           MOVE x   TO b      IN AGroup
           MOVE x   TO b OF a IN AGroup

           .

       END PROGRAM Booleans.
