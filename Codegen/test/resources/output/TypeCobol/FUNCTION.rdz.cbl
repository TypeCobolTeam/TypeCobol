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
       01 POW-result PIC 9(08).
       01 ERROR-CODE PIC X(08).


       PROCEDURE DIVISION.
       
      *DECLARE function POW PRIVATE
      *      INPUT x PIC 9(05)
      *            y PIC 9(03)
      *      RETURNING result PIC 9(08)
      *  .

       TRAITEMENT.
      *    MOVE FUNCTION POW (x y)    TO x

           CALL 'ad9bc8ae' USING
                    BY REFERENCE x
                    BY REFERENCE y

                    BY REFERENCE POW-RESULT
                    BY REFERENCE ERROR-CODE

           IF ERROR-CODE = ZERO
               MOVE POW-RESULT TO x
           ELSE
      *    TODO: error management
           END-IF
           .

       END PROGRAM Functions.
      *
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ad9bc8ae.
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
       END PROGRAM ad9bc8ae.
