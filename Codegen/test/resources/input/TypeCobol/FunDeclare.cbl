       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       
       PROCEDURE DIVISION.
      *code généré pour gérer les pointeurs
            .
       
       DECLARE function DoesNothing PUBLIC.
         PROCEDURE DIVISION.
           DISPLAY 'I DO NOTHING'
           .
       END-DECLARE.

       DECLARE function ReturnsZero PUBLIC.
         DATA DIVISION.
         LINKAGE SECTION.
           01 result PIC 9(04).
         PROCEDURE DIVISION
             RETURNING result.
           MOVE 0 TO result.
           .
       END-DECLARE.
       
       DECLARE function StrangelyReturnsItsInput PRIVATE.
         DATA DIVISION.
         LINKAGE SECTION.
           01 x PIC 9(04).
           01 result PIC 9(04).
         PROCEDURE DIVISION
             INPUT x
             RETURNING result
         .
           IF x = 0
             MOVE 0 TO result
           ELSE
             MOVE x TO result
           END-IF.
       END-DECLARE.
       
       DECLARE function SumThreeWithClutterInLinkage PRIVATE.
         DATA DIVISION.
         LINKAGE SECTION.
           01 x PIC 9(04).
           01 y PIC 9(04).
           01 z PIC 9(04).
           01 a PIC 9(04).
           01 b PIC 9(04).
           01 c PIC 9(04).
           01 result PIC 9(04).
         PROCEDURE DIVISION
             INPUT x y z
             RETURNING result
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
         LINKAGE SECTION.
           01 x PIC 9(04).
           01 y PIC 9(04).
         PROCEDURE DIVISION
             INPUT  x y
             OUTPUT x y
         .
           MOVE x TO tmp
           MOVE y TO x
           MOVE tmp TO y
           .
       END-DECLARE.

       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
       END PROGRAM FunDeclare.
       