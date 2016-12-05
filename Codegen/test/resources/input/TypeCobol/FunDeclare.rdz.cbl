       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       SERVICE IS YFUNCOPY.
       
       DATA DIVISION.
       FILE SECTION.
       LOCAL-STORAGE SECTION.

       PROCEDURE DIVISION.
           .
       
       DECLARE FUNCTION DoesNothing PUBLIC.
         PROCEDURE DIVISION.
           DISPLAY 'I DO NOTHING'
           .
       END-DECLARE.

       DECLARE FUNCTION ReturnsZero PUBLIC
             RETURNING result PIC 9(04).
       DATA DIVISION.
       PROCEDURE DIVISION.
           MOVE 0 TO result.
           .
       END-DECLARE.

      *OK: second function with same name, but profile is different
       DECLARE FUNCTION DoesNothing PUBLIC
             INPUT x PIC 9(04).
       PROCEDURE DIVISION.
           DISPLAY 'I DO NOTHING WITH ' x
           .
       END-DECLARE.

      * ERROR Illegal FILE SECTION
       DECLARE FUNCTION StrangelyReturnsItsInput PUBLIC
             INPUT     x      PIC 9(04)
             RETURNING result PIC 9(04)
           .
       DATA DIVISION.
       FILE SECTION.
       FD myfile. 01 toto PIC X.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
           IF x = 0
             MOVE 0 TO result
           ELSE
             MOVE x TO result
           END-IF.
       END-DECLARE.

      * ERROR because x,y, a.x,a.z and result shouldn't be in LINKAGE
       DECLARE FUNCTION SumThreeWithClutterInLinkage PUBLIC
             INPUT x PIC 9(04)
                   y PIC 9(04)
                   z PIC 9(04)
                   t PIC 9(04)
             RETURNING result PIC 9(04)
         .
       DATA DIVISION.
       LINKAGE SECTION.
       01 x PIC 9(04).
       01 y PIC 9(02).
       01 a PIC 9(04).
           05 x PIC 9(02).
           05 z PIC 9(02).
       01 b PIC 9(04).
       01 c PIC 9(04).
       01 result PIC 9(04).
       PROCEDURE DIVISION.
           MOVE 0 TO result.
           ADD x to result.
           ADD y to result.
           ADD z to result.
       END-DECLARE.
       
       DECLARE PROCEDURE SwapParameters PRIVATE
             INOUT x PIC 9(04)
                   y PIC 9(04)
         .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 tmp PIC 9(04).
       PROCEDURE DIVISION.
           MOVE x TO tmp
           MOVE y TO x
           MOVE tmp TO y
           .
       END-DECLARE.

      * ERROR because x and y should be INOUT
      * ERROR because y INPUT vs OUTPUT types differ
       DECLARE PROCEDURE SwapParametersWrong PRIVATE
             INPUT  x PIC 9(04)
                    y PIC 9(04)
                    a PIC 9(04)
             OUTPUT x PIC 9(04)
                    y PIC 9(08)
                    b PIC 9(04)
         .
         PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.
      * ERROR because illegal GLOBAL or EXTERNAL
       DECLARE FUNCTION IllegalClauses PUBLIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 g PIC X IS GLOBAL.
       01 e PIC X IS EXTERNAL.
       PROCEDURE DIVISION.
           .
       END-DECLARE.

       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
       DECLARE FUNCTION FunConditions PRIVATE
             INPUT  gender PIC X(01)
                 88  valid-gender VALUE 'F' 'M'
                 88  female VALUE 'F'
                 88  male   VALUE 'M'
         .
       PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.
      * ERROR level-88 parameter items must be subordinate to another item
      * ERROR only level-88 parameter items shall have an explicit level number
       DECLARE FUNCTION FunConditions PRIVATE
             INPUT 88 valid-gender VALUE 'F' 'M'
                      gender PIC X(01)
                   88  female VALUE 'F'
                   01  male   VALUE 'M'
         .
       PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.
       
       DECLARE FUNCTION MyNOT PRIVATE
             INPUT     x type BOOL
             RETURNING y TYPE bool
         .
       PROCEDURE DIVISION.
           IF NOT x
             SET y TO TRUE
           ELSE
             SET y TO FALSE
           END-IF.
       END-DECLARE.
       
       END PROGRAM FunDeclare.