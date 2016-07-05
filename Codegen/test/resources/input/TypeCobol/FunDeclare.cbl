       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       
       PROCEDURE DIVISION.
       
       DECLARE FUNCTION Fun0 PUBLIC
         DATA DIVISION.
         LINKAGE SECTION.
           01 result PIC 9(04).
         PROCEDURE DIVISION RETURNING result.
           GOBACK result 3
           .
       END-DECLARE.
       
       DECLARE FUNCTION Fun1 PRIVATE
         DATA DIVISION.
         LINKAGE SECTION.
           01 x PIC 9(04).
           01 result PIC 9(04).
         PROCEDURE DIVISION
             INPUT x
             RETURNING result
         .
           IF x = 0
             GOBACK result 2
           ELSE
             GOBACK result x
           END-IF.
       END-DECLARE.
       
       DECLARE FUNCTION Fun2 PRIVATE
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
       
       DECLARE FUNCTION Fun3 PRIVATE
         DATA DIVISION.
         LINKAGE SECTION.
           01 x PIC 9(04).
           01 y PIC 9(04).
         WORKING-STORAGE SECTION.
           01 tmp PIC 9(04).
         PROCEDURE DIVISION
             INPUT  x y
             OUTPUT x y
         .
           SET tmp TO x
       	   SET x TO y
       	   SET y TO tmp
           .
       END-DECLARE.
       
       END PROGRAM FunDeclare.
       