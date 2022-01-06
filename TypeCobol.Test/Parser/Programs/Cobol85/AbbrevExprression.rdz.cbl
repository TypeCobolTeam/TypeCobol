       Identification division.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-Var1 pic X(05).
       01  A pic X(05).
       01  B pic X(05).
       01  C pic X(05).
       01  D pic X(05).
      
       PROCEDURE DIVISION.
           IF   W-Var1 = ('a' OR 'b')
           OR          = ('c' OR 'd')
               display "hello"
           END-IF
      
           evaluate true
             when (A = B AND NOT < C OR D)
             when A NOT > B OR C
             when NOT A = B OR C
             when NOT (A = B OR < C)
             when NOT (A NOT = B AND C AND NOT D)
               display "hello"
             when other
                continue
           end-evaluate
           .
       END PROGRAM MyPGM.