       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestingPgm.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 W-Date TYPE DATE.
       01 W-Bool TYPE Bool.
       01 W-Numeric PIC 9(10).
       01 W-Alphabetic PIC A(10).
       01 W-Alphanum PIC X(10).
       01 W-OutDate TYPE Date.
       01 TestLevel.
           88 TestLevel88 VALUE 1.
       01 Level1.
          05 Level2.
            06 VarLevel2 PIC 10.
          05 Level2-2.
            06 VarLevel2-2 PIC 10.
          05 MyBool TYPE BOOL.
       PROCEDURE DIVISION.
       SectionEmpty SECTION.
 
       SectionEmpty2 SECTION.
       .
        
       Section1 SECTION.
       .
       Parag1.
       .
      
       Parag2.
       .
      
       Parag3.
       .
      
       PERFORM
       MOVE Level1::Level2 TO Level1::Level2-2.
      
       Parag4.
       .
      
       Parag5.
       .
      
      
      
       .
       Parag6.
       END PROGRAM TestingPgm.
       