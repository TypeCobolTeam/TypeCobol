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
       PROGRAM-ID. c49a4761.
       PROCEDURE DIVISION
           .
      *FunDeclare.DoesNothing  - No Params
           DISPLAY 'I DO NOTHING'
           .
       END PROGRAM c49a4761.
      *
      *DECLARE FUNCTION ReturnsZero PRIVATE
      *      RETURNING result PIC 9(04).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. e61a1c43.
       DATA DIVISION.
       LINKAGE SECTION.
      *FunDeclare.ReturnsZero  - No Params
      *     returns(result: pic 9(04))
       01 result PIC 9(04).
       PROCEDURE DIVISION
             USING BY REFERENCE result
           .
      *FunDeclare.ReturnsZero  - No Params
      *     returns(result: pic 9(04))
           MOVE 0 TO result.
           .
       END PROGRAM e61a1c43.
      *
      *DECLARE FUNCTION DoesNothing PRIVATE
      *      INPUT x PIC 9(04).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cd51a7fd.
       DATA DIVISION.
       LINKAGE SECTION.
      *FunDeclare.DoesNothing - Params :
      *     input(x: pic 9(04))
       01 x PIC 9(04).
       PROCEDURE DIVISION
             USING BY REFERENCE x
           .
      *FunDeclare.DoesNothing - Params :
      *     input(x: pic 9(04))
           DISPLAY 'I DO NOTHING WITH ' x
           .
       END PROGRAM cd51a7fd.
      *
      *DECLARE FUNCTION StrangelyReturnsItsInput PRIVATE
      *      INPUT     x      PIC 9(04) comp-3
      *      RETURNING result PIC 9(04) comp
      *    .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. c498f2f1.
       DATA DIVISION.
       LINKAGE SECTION.
      *FunDeclare.StrangelyReturnsItsInput - Params :
      *     input(x: pic 9(04) comp-3)
      *     returns(result: pic 9(04) comp)
                       
       01 x PIC 9(04) comp-3.
       01 result PIC 9(04) comp.
       PROCEDURE DIVISION
             USING BY REFERENCE x
                   BY REFERENCE result
           .
      *FunDeclare.StrangelyReturnsItsInput - Params :
      *     input(x: pic 9(04) comp-3)
      *     returns(result: pic 9(04) comp)
           IF x = 0
             MOVE 0 TO result
           ELSE
             MOVE x TO result
           END-IF.
       END PROGRAM c498f2f1.
      *
      *declare function UseACopy private
      *         input  x pic X.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. e7b552c0.
       data division.
       working-storage section.
      *FunDeclare.UseACopy - Params :
      *     input(x: pic X)
                               
       01 yoto pic X.
       REPLACE ==:MyPrefix:== by ==MyPrefix2==.
       01 MyCopy. COPY MyDataCopy.
       LINKAGE SECTION.
      *FunDeclare.UseACopy - Params :
      *     input(x: pic X)
       01 x pic X.
       PROCEDURE DIVISION
             USING BY REFERENCE x
           .
      *FunDeclare.UseACopy - Params :
      *     input(x: pic X)
           display "Hello"
           COPY MyProcedureCopy.
           .
       END PROGRAM e7b552c0.
