       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       PROCEDURE DIVISION.

      *DECLARE FUNCTION DoesNothing PRIVATE.

      *DECLARE FUNCTION ReturnsZero PRIVATE
      *      RETURNING result PIC 9(04).

      *OK: second function with same name, but profile is different
      *DECLARE FUNCTION DoesNothing PRIVATE
      *      INPUT x PIC 9(04).

      *DECLARE FUNCTION StrangelyReturnsItsInput PRIVATE
      *      INPUT     x      PIC 9(04) comp-3
      *      RETURNING result PIC 9(04) comp
      *    .

      *written in lower-case to make sure code generation doesn't 
      *change it to upper-case
      *declare function UseACopy private
      *         input  x pic X.


       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
       END PROGRAM FunDeclare.
      *
      *DECLARE FUNCTION DoesNothing PRIVATE.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. c49a4761DoesNothing.
       PROCEDURE DIVISION
           .
           DISPLAY 'I DO NOTHING'
           .
       END PROGRAM c49a4761DoesNothing.
      *
      *DECLARE FUNCTION ReturnsZero PRIVATE
      *      RETURNING result PIC 9(04).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. e61a1c43ReturnsZero.
       DATA DIVISION.
       LINKAGE SECTION.
       01 result PIC 9(04).
       PROCEDURE DIVISION
             USING BY REFERENCE result
           .
           MOVE 0 TO result.
           .
       END PROGRAM e61a1c43ReturnsZero.
      *
      *DECLARE FUNCTION DoesNothing PRIVATE
      *      INPUT x PIC 9(04).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cd51a7fdDoesNothing.
       DATA DIVISION.
       LINKAGE SECTION.
       01 x PIC 9(04).
       PROCEDURE DIVISION
             USING BY REFERENCE x
           .
           DISPLAY 'I DO NOTHING WITH ' x
           .
       END PROGRAM cd51a7fdDoesNothing.
      *
      *DECLARE FUNCTION StrangelyReturnsItsInput PRIVATE
      *      INPUT     x      PIC 9(04) comp-3
      *      RETURNING result PIC 9(04) comp
      *    .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. c498f2f1StrangelyReturnsItsInp.
       DATA DIVISION.
       LINKAGE SECTION.
       01 x PIC 9(04) comp-3.
       01 result PIC 9(04) comp.
       PROCEDURE DIVISION
             USING BY REFERENCE x
                   BY REFERENCE result
           .
           IF x = 0
             MOVE 0 TO result
           ELSE
             MOVE x TO result
           END-IF.
       END PROGRAM c498f2f1StrangelyReturnsItsInp.
      *
      *declare function UseACopy private
      *         input  x pic X.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. e7b552c0UseACopy.
       data division.
       working-storage section.
       01 yoto pic X.
       REPLACE ==:MyPrefix:== by ==MyPrefix2==.
       COPY MyDataCopy.
       LINKAGE SECTION.
       01 x PIC X.
       PROCEDURE DIVISION
             USING BY REFERENCE x
           .
           display "Hello"
           COPY MyProcedureCopy.
           .
       END PROGRAM e7b552c0UseACopy.
