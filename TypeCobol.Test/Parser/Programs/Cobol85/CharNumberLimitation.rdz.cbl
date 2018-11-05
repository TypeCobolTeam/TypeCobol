       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *01  MyVarThatHasAVeryLongNameSoItWontFitToTheEightyCobolCharacterLimitation pic X(09).
       01  MyVarFollowedByWhitespaces  pic X(50).                                             
       01  MyVar pic X.
            88 MyVar-A value "A".
            88 MyVar-B value "B".
            
       PROCEDURE DIVISION.
       
           IF MyVar-A
                move "mlkjjmlkjmlkjmlkjmlkjmlkjmlkjmlkjmlkjmlkjmlkjmlk" to MyVarFollowedByWhitespaces.
                continue
           else
                continue
           end-if
           .
      
       END PROGRAM PGM1.