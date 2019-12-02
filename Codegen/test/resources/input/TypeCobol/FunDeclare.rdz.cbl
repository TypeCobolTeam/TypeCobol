       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       PROCEDURE DIVISION.

       DECLARE FUNCTION DoesNothing PRIVATE.
         PROCEDURE DIVISION.
           DISPLAY 'I DO NOTHING'
           .
       END-DECLARE.

       DECLARE FUNCTION ReturnsZero PRIVATE
             RETURNING result PIC 9(04).
       DATA DIVISION.
       PROCEDURE DIVISION.
           MOVE 0 TO result.
           .
       END-DECLARE.

      *OK: second function with same name, but profile is different
       DECLARE FUNCTION DoesNothing PRIVATE
             INPUT x PIC 9(04).
       PROCEDURE DIVISION.
           DISPLAY 'I DO NOTHING WITH ' x
           .
       END-DECLARE.

       DECLARE FUNCTION StrangelyReturnsItsInput PRIVATE
             INPUT     x      PIC 9(04) comp-3
             RETURNING result PIC 9(04) comp
           .
       DATA DIVISION.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
           IF x = 0
             MOVE 0 TO result
           ELSE
             MOVE x TO result
           END-IF.
       END-DECLARE.

      *written in lower-case to make sure code generation doesn't 
      *change it to upper-case
       declare function UseACopy private
                input  x pic X.
       data division.
       working-storage section.
       01 yoto pic X.
       REPLACE ==:MyPrefix:== by ==MyPrefix2==.
       COPY MyDataCopy.
       procedure division.
           display "Hello"
           COPY MyProcedureCopy.
           .
       end-declare.


       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
       END PROGRAM FunDeclare.