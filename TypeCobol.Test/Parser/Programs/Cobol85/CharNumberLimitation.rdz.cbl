       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01  MyDateThatHasAVeryLongNameSoItWontFitToTheEightyCobolCharacterLimitation TYPE Date.
       01  MyDateTypeFollowedByWhitespaces  TYPE Date.                                                               
       01 MyVar pic X.
          88 MyVar-A value "A".
          88 MyVar-B value "B".
      
       PROCEDURE DIVISION.
           IF MyVar-A                                                                                     
                    MOVE "mlkjmlkjmljmlkjmlkjmlkjmlkjmljmlkjmlkjmlkjmlkjmlkjmlkjmlkjmlkjmlkj"
                    CONTINUE
           ELSE
                    CONTINUE
           END-IF
      
       END PROGRAM PGM1.