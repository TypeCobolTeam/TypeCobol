       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFTIMI.
       ENVIRONMENT DIVISION.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       PROCEDURE DIVISION.
       PARA-7A.
           display "A"
           GO TO P9901
           .
       P9901.
           GO TO P9902
           .
       P9902.
           display "C"
           ALTER P9901 TO P9903
           GO TO PARA-7A
           .
       P9903.
           display "D"
           GO TO PARA-7B
           .
       PARA-7B.
           GOBACK
           .
       END PROGRAM PERFTIMI.
