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
      *      10  b TYPE BOOL.
             10  b-value PIC X VALUE LOW-VALUE.
         88  b       VALUE 'T'.
         88  b-false VALUE 'F'.
                             
      *    05  d TYPE BOOL.
           05  d-value PIC X VALUE LOW-VALUE.
         88  d       VALUE 'T'.
         88  d-false VALUE 'F'.
                           

000000 01 mygroup.
000000    05 array occurs 5.
      *      10 checkToDo type bool.
000000       10  checkToDo-value PIC X VALUE LOW-VALUE.
         88  checkToDo       VALUE 'T'.
         88  checkToDo-false VALUE 'F'.
                                    

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
      *    set checkToDo(1) to false
           SET checkToDo(1)-false TO TRUE.
                                    
           .

       END PROGRAM Booleans.
