       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       
       PROCEDURE DIVISION.
            .
       
       DECLARE function DoesNothing PUBLIC.
         PROCEDURE DIVISION.
           DISPLAY 'I DO NOTHING'
           .
       END-DECLARE.

       DECLARE function ReturnsZero PUBLIC.
         DATA DIVISION.
         PROCEDURE DIVISION
             RETURNING result PIC 9(04).
           MOVE 0 TO result.
           .
       END-DECLARE.

      * ERROR Illegal FILE SECTION
       DECLARE function StrangelyReturnsItsInput PRIVATE.
         DATA DIVISION.
         FILE SECTION.
           FD myfile. 01 toto PIC X.
         LINKAGE SECTION.
         PROCEDURE DIVISION
             INPUT     x      PIC 9(04)
             RETURNING result PIC 9(04)
         .
           IF x = 0
             MOVE 0 TO result
           ELSE
             MOVE x TO result
           END-IF.
       END-DECLARE.

      * ERROR because x, y and result shouldn't be in LINKAGE
       DECLARE function SumThreeWithClutterInLinkage PRIVATE.
         DATA DIVISION.
         LINKAGE SECTION.
           01 x PIC 9(04).
           01 y PIC 9(04).
           01 a PIC 9(04).
           01 b PIC 9(04).
           01 c PIC 9(04).
           01 result PIC 9(04).
         PROCEDURE DIVISION
             INPUT x PIC 9(04).
                   y PIC 9(04)
                   z PIC 9(04).
             RETURNING result PIC 9(04)
         .
           MOVE 0 TO result.
           ADD x to result.
           ADD y to result.
           ADD z to result.
       END-DECLARE.
       
       DECLARE function SwapParameters PRIVATE.
         DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 tmp PIC 9(04).
         PROCEDURE DIVISION
             INOUT x PIC 9(04)
                   y PIC 9(04)
         .
           MOVE x TO tmp
           MOVE y TO x
           MOVE tmp TO y
           .
       END-DECLARE.

      * ERROR because x and y should be INOUT
      * ERROR because y INPUT vs OUTPUT types differ
       DECLARE function SwapParametersWrong PRIVATE.
         PROCEDURE DIVISION
             INPUT  x PIC 9(04)
                    y PIC 9(04).
                    a PIC 9(04)
             OUTPUT x PIC 9(04).
                    y PIC 9(08).
                    b PIC 9(04).
         .
           CONTINUE.
       END-DECLARE.
      * ERROR because illegal GLOBAL or EXTERNAL
       DECLARE function IllegalClauses PUBLIC.
         DATA DIVISION.
           WORKING-STORAGE SECTION.
             01 x PIC X IS GLOBAL.
             01 y PIC X IS EXTERNAL.
         PROCEDURE DIVISION.
           .
       END-DECLARE.

       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
       DECLARE function FunConditions PRIVATE.
         PROCEDURE DIVISION
             INPUT  gender PIC X(01).
                 88  valid-gender VALUE 'F' 'M'.
                 88  female VALUE 'F'.
                 88  male   VALUE 'M'.
           CONTINUE.
       END-DECLARE.
       
       END PROGRAM FunDeclare.