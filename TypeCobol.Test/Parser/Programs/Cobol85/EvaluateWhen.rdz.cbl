       IDENTIFICATION DIVISION.
       PROGRAM-ID. EvalWhen.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.

           evaluate true
             when nullInd < x'80'
               display "test"
           end-evaluate.

       END PROGRAM EvalWhen.