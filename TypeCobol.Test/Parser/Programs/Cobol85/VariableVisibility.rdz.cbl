       IDENTIFICATION DIVISION.
       PROGRAM-ID. MainProgram.
       data division.
       working-storage section.
       01 dataMain1 pic X.
       01 dataMain2 pic X global.
      
       procedure division.
      *KO dataNested21 and dataNested22 don't exist in MainProgram
            move dataNested21 to dataNested22
      *KO dataNested1 and dataNested2 don't exist in MainProgram
            move dataNested1 to dataNested2
      *OK 
            move dataMain1 to dataMain2
            .
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested.
       data division.
       working-storage section.
       01 dataNested1 pic X.
       01 dataNested2 pic X global.
       PROCEDURE DIVISION.
      *KO dataNested21 and dataNested22 don't exist in Nested
            move dataNested21 to dataNested22
      *OK 
            move dataNested1 to dataNested2
      *KO dataMain1 is not declared as global
            move dataMain1 to dataMain2
            .
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested2.
       data division.
       working-storage section.
       01 dataNested21 pic X.
       01 dataNested22 pic X global.
      
       PROCEDURE DIVISION.
      *OK
            move dataNested21 to dataNested22
      *KO dataNested1 is not declared as global
            move dataNested1 to dataNested2
      *KO dataMain1 is not declared as global
            move dataMain1 to dataMain2
            .
       END PROGRAM Nested2.
       END PROGRAM Nested.
       END PROGRAM MainProgram.