      * 7 CodeElements errors
      * "1"@(15:?>15:?): [27:1] Syntax error : Group items should not contain TYPE BOOL items      <--------- TODO
      * "1"@(18:?>18:?): [27:1] Syntax error : TYPE BOOL should not be subordinate to another item <--------- TODO
      * "1"@(19:?>19:?): [27:1] Syntax error : TYPE BOOL should not be subordinate to another item <--------- TODO
      * "1"@(35:?>35:?): [27:1] Syntax error : Illegal write to Identifier                         <--------- TODO
      * "1"@(37:?>37:?): [27:1] Syntax error : BOOL properties are read-only                       <--------- TODO
      * "1"@(38:?>38:?): [27:1] Syntax error : BOOL properties are read-only                       <--------- TODO
      * "1"@(46:?>46:?): [27:1] Syntax error : Group contains TYPE BOOL variables                  <--------- TODO
      * "1"@(47:?>47:?): [27:1] Syntax error : Group contains TYPE BOOL variables                  <--------- TODO
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Booleans.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  x PIC X.
      * Booleans declaration
       01  Identifier-value PIC X VALUE LOW-VALUE.
           88  Identifier       VALUE 'T'.
           88  Identifier-false VALUE 'F'.
       01  AnotherOne-value PIC X VALUE LOW-VALUE.
           88  AnotherOne       VALUE 'T'.
           88  AnotherOne-false VALUE 'F'.
      * WARNING: initialization of a group containing booleans
       01  AGroup.
           05 a PIC X.
             10 a PIC X.
             10 b-value PIC X VALUE LOW-VALUE.
               88  b       VALUE 'T'.
               88  b-false VALUE 'F'.
           05 b-value PIC X VALUE LOW-VALUE.
               88  b       VALUE 'T'.
               88  b-false VALUE 'F'.




       PROCEDURE DIVISION.

       TRAITEMENT.
           SET Identifier  TO TRUE
           SET Identifier  TO FALSE
		   
           MOVE TRUE         TO Identifier
           MOVE FALSE        TO Identifier
           MOVE AnotherOne   TO Identifier
           MOVE Identifier   TO x
      * ERROR: a boolean can only receive booleans, TRUE or FALSE
           MOVE x   TO Identifier
      * ERROR: a boolean subordinates are read-only
           MOVE x   TO Identifier-value
           MOVE x   TO Identifier-false OF Identifier-value

           MOVE x   TO a      IN AGroup
           MOVE x   TO a OF a IN AGroup
           MOVE Identifier    TO b      IN AGroup
           MOVE Identifier    TO b OF a IN AGroup
           MOVE b IN AGroup   TO b OF a IN AGroup
      * WARNING: moving to a group containing booleans
           MOVE x   TO b      IN AGroup
           MOVE x   TO b OF a IN AGroup

           .

       END PROGRAM Booleans.
