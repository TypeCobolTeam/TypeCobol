       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DVZF0SOM.
       data division.
       working-storage section.
       01 Var1 pic X(10).
       PROCEDURE DIVISION.           
           EVALUATE Var1
               WHEN  SPACES
                  CONTINUE
      * OK
               WHEN ALL '*'
                  display "*"
      * OK
               WHEN ALL 'A'
                  display "A"
               WHEN OTHER
                  display "other"
           END-EVALUATE
           .
       END PROGRAM DVZF0SOM.