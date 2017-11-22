       IDENTIFICATION DIVISION.
       PROGRAM-ID. Functions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  x PIC 9.
       01  y PIC 9(3).
       01  z PIC 9(5).
      *01  b TYPE BOOL.
       01  b-value PIC X VALUE LOW-VALUE.
           88  b       VALUE 'T'.
           88  b-false VALUE 'F'.
                       

       PROCEDURE DIVISION.
       
      *DECLARE function POW PRIVATE
      *      INPUT x PIC 9(05)
      *            y PIC 9(03)
      *      RETURNING result PIC 9(08)
      *  .

       TRAITEMENT.
      *    function are not fully implemented
      *    MOVE FUNCTION POW (x y)    TO x
           .

       END PROGRAM Functions.
      *
      *DECLARE function POW PRIVATE
      *      INPUT x PIC 9(05)
      *            y PIC 9(03)
      *      RETURNING result PIC 9(08)
      *  .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. c40368ddPOW.
       DATA DIVISION.
       LINKAGE SECTION.
       01 x PIC 9(05).
       01 y PIC 9(03).
       01 result PIC 9(08).
       PROCEDURE DIVISION
             USING BY REFERENCE x
                   BY REFERENCE y
                   BY REFERENCE result
           .
           CONTINUE.
       END PROGRAM c40368ddPOW.
