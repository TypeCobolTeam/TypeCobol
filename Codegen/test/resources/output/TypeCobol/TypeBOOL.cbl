      * 12 CodeElements errors
      * "1"@(13:8>13:18): [27:1] Syntax error : Group items should not contain non nestable type BOOL items
      * "1"@(13:8>13:18): [27:1] Syntax error : Group items should not contain non nestable type BOOL items
      * "1"@(14:10>14:21): [27:1] Syntax error : Group items should not contain non nestable type BOOL items
      * "1"@(16:12>16:27): [27:1] Syntax error : Type BOOL should not be subordinate to another item
      * "1"@(17:10>17:25): [27:1] Syntax error : Type BOOL should not be subordinate to another item
      * "1"@(25:12>25:34): [27:1] Syntax error : Strongly typed variable can't be used as a receiving operand of SetStatement
      * "1"@(26:12>26:35): [27:1] Syntax error : Strongly typed variable can't be used as a receiving operand of SetStatement
      * "4"@(33:12>33:33): [30:1] Semantic error: Can't write non typed Alphanumeric to strongly typed variable Identifier:BOOL (use unsafe keyword for that)
      * "1"@(35:12>35:39): [27:1] Syntax error : Symbol Identifier-value is not referenced
      * "1"@(36:12>36:59): [27:1] Syntax error : Symbol Identifier-value.Identifier-false is not referenced
      * "1"@(43:12>43:34): [27:1] Syntax error : Group contains type BOOL variables
      * "1"@(44:12>44:34): [27:1] Syntax error : Group contains type BOOL variables
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
                                                                              
      * WARNING: initialization of a group containing booleans
       01  AGroup.
         05  a PIC X.
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
      * KO: a boolean subordinates are read-only
           MOVE x   TO Identifier-value
           MOVE x   TO Identifier-false OF Identifier-value
      * OK
           MOVE x   TO c OF a IN AGroup
           MOVE Identifier    TO d      IN AGroup
           MOVE Identifier    TO b OF a IN AGroup
           MOVE d IN AGroup   TO b OF a IN AGroup
      * KO: moving to a group containing booleans
           MOVE x   TO      AGroup
           MOVE x   TO a IN AGroup
           .

       END PROGRAM Booleans.
