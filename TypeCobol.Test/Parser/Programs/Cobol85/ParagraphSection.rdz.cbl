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

      *** Testing perform statements in procedure division
      *OK PAR02 is uniquely defined in SEC02
           Perform PAR02
      
      *KO PAR05 is declared multiple times in this scope (PROCEDURE DIVISION)
           Perform PAR05

      *OK PAR01 is declared only once in this scope (PROCEDURE DIVISION)
           Perform PAR01 thru PAR02.

      *OK
           Perform PAR01 thru PAR02 OF SEC02.

      *KO ambiguous reference between SEC01.PAR04 and SEC02.PAR04
           Perform PAR04.

      *KO ambiguous reference between section SEC05 and paragraph SEC06.SEC05
           Perform SEC05.

      *OK name SEC05 is qualified
           Perform SEC05 OF SEC06.

      *KO ambiguous reference to SEC07 defined multiple times
           Perform SEC07.

      *** Testing perform statements in a parapraph of procedure division
       TEST-PERFORM-PROCEDURE-DIVISION.
      *OK PAR02 is uniquely defined in SEC02
           Perform PAR02
      
      *KO PAR05 is declared multiple times in this scope (PROCEDURE DIVISION)
           Perform PAR05

      *OK PAR01 is declared only once in this scope (PROCEDURE DIVISION)
           Perform PAR01 thru PAR02.

      *OK
           Perform PAR01 thru PAR02 OF SEC02.

      *KO ambiguous reference between SEC01.PAR04 and SEC02.PAR04
           Perform PAR04.
           .
      
      *OK PAR01 is declared only once in procedure division
       PAR01.
           continue.
           .
      *OK PAR03 is already defined but in SEC01
       PAR03.
           continue.
           .
      *KO PAR05 already declared in procedure division
       PAR05.
           continue.
           .
      *KO PAR05 already declared in procedure division
       PAR05.
           continue.
           .

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

      *KO PAR01 is already declared in section SEC01
       PAR01.
           continue
           .
      *OK PAR03 is already defined but in procedure division
       PAR03.
           continue.
           .
      *OK PAR04 is already defined but in another section
       PAR04.
           continue.
           .
      *KO PAR01 is already declared in section SEC01
       PAR01.
           continue
           .

       SEC02 SECTION.
      *OK, PAR01 is already defined but in another section
       PAR01.
           continue
           .
      *OK PAR02 is uniquely defined
       PAR02.
           continue
           .
      *OK PAR04 is already defined but in another section
       PAR04.
           continue.
           .

      *** Testing perform statements in section
      *OK PAR01 and PAR02 are uniquely defined in SEC02
           Perform PAR01 thru PAR02.

      *KO ambiguous reference to SEC01.PAR01 
           Perform PAR01 OF SEC01.

      *KO PAR02.SEC01 is not defined
           Perform PAR02 OF SEC01.

      *KO can't resolve between SEC01.PAR03 and PROCEDURE DIVISION.PAR03
           Perform PAR03.

      *OK PAR03 is qualified
           Perform PAR03 OF SEC01 thru PAR04.

      *** Testing perform statements in a parapraph of a section
       TEST-PERFORM-SEC02.
      *OK PAR01 and PAR02 are uniquely defined in SEC02
           Perform PAR01 thru PAR02.

      *KO ambiguous reference to SEC01.PAR01 
           Perform PAR01 OF SEC01.

      *KO PAR02.SEC01 is not defined
           Perform PAR02 OF SEC01.

      *KO can't resolve between SEC01.PAR03 and PROCEDURE DIVISION.PAR03
           Perform PAR03.

      *OK PAR03 is qualified
           Perform PAR03 OF SEC01 thru PAR04.
           .

       SEC03 SECTION.
       TEST-PERFORM-SEC03.
      *KO can't resolve between SEC01.PAR04 and SEC02.PAR04
           Perform PAR04.
      *KO can't resolve between SEC01.PAR04 and SEC02.PAR04
           Perform PAR02 thru PAR04
      *OK PAR02 is uniquely defined and PAR04 is qualified
           Perform PAR02 thru PAR04 OF SEC01
           .

      *KO SEC04 already declared as paragraph
       SEC04 SECTION.
      *KO SEC04 already declared as section
       SEC04.
           continue
           .

      *KO section SEC05 already declared as a paragraph
       SEC05 SECTION.
       PAR06. 
            continue
            .
      *KO ambiguous reference between section SEC05 and paragraph SEC06.SEC05
           Perform SEC05.

       SEC06 SECTION.
      *KO ambiguous reference between section SEC05 and paragraph SEC06.SEC05
           Perform SEC05.
      *OK name SEC05 is qualified
           Perform SEC05 OF SEC06.
      *KO paragraph SEC05 is already declared as a section
       SEC05. 
            continue
            .
       PAR07.
      *KO ambiguous reference to SEC07 defined multiple times
            Perform SEC07.
            .

      *Warning SEC07 declared more than once
       SEC07 SECTION.
       PAR07.
            continue
            .

      *Warning SEC07 declared more than once
       SEC07 SECTION.
       PAR08.
            continue
            .

       END PROGRAM TestingPgm.