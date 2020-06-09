       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestingPgm.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Level1.
          05 Level2.
            06 VarLevel2 PIC X(10).
          05 Level2-2.
            06 VarLevel2-2 PIC X(10).
          05 MyBool TYPE BOOL.
       PROCEDURE DIVISION.
       INIT-LIBRARY.
           exit.
       SectionEmpty SECTION.
      
       SectionEmpty2 SECTION.
           .
      
       Section1 SECTION.
           .
       Parag1.
           EXEC SQL OPEN something END-EXEC
           .
      
       Parag2.
           .
      
       Parag3.
           .
      
           MOVE Level1::Level2 TO Level1::Level2-2.
      
       Parag4.
           .
      
       Parag5.
           .
      
           .
       Parag6.

       SEC01 SECTION.

      *KO PAR01 is already declared
       PAR01.
           continue
           .
       
       PAR02.
           continue
           .
      *KO PAR01 is already declared
       PAR01.
           continue
           .

       SEC02 SECTION.

       PAR01.
           continue
           .

       PAR02.
           continue
           .

       END PROGRAM TestingPgm.