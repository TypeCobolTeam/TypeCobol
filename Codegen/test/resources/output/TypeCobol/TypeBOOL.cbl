      * 5 CodeElements errors
      * "4"@(31:12>31:33): [30:1] Semantic error: Cannot write Alphanumeric to strongly typed variable Identifier:BOOL (use UNSAFE keyword for that)
      * "4"@(32:12>32:39): [30:1] Semantic error: Cannot write Alphanumeric to strongly typed variable b:BOOL (use UNSAFE keyword for that)
      * "4"@(33:12>33:39): [30:1] Semantic error: Cannot write Alphanumeric to strongly typed variable d:BOOL (use UNSAFE keyword for that)
      * "1"@(35:12>35:39): [27:1] Syntax error : Symbol Identifier-value is not referenced
      * "1"@(36:12>36:59): [27:1] Syntax error : Symbol Identifier-value.Identifier-false is not referenced
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Booleans.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  x PIC X.
      * Booleans declaration
      *01  Identifier TYPE BOOL.
       01  Identifier-value PIC X VALUE LOW-VALUE.
         88  Identifier       VALUE 'T'.
         88  Identifier-false VALUE 'F'.
      *01  AnotherOne TYPE BOOL.
       01  AnotherOne-value PIC X VALUE LOW-VALUE.
         88  AnotherOne       VALUE 'T'.
         88  AnotherOne-false VALUE 'F'.
       01  AGroup.
         05  a.
           10  c PIC X.
      *    10  b TYPE BOOL.
           10  b-value PIC X VALUE LOW-VALUE.
             88  b       VALUE 'T'.
             88  b-false VALUE 'F'.
      *  05  d TYPE BOOL.
         05  d-value PIC X VALUE LOW-VALUE.
           88  d       VALUE 'T'.
           88  d-false VALUE 'F'.
       
       
       
       PROCEDURE DIVISION.
       
       TRAITEMENT.
           SET Identifier  TO TRUE
      *    SET Identifier  TO FALSE
           SET Identifier-false TO TRUE.
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
