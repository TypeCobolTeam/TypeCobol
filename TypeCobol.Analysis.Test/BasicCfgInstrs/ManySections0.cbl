       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXITSEC0.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I PIC 9(4) COMP.
       01 J PIC 9(4) COMP.
       PROCEDURE DIVISION.

       PROC1.
               PERFORM LABEL1 THRU E--LABEL1
               PERFORM LABEL4 THRU E--LABEL4
               CONTINUE.
       E--PROC1.
               EXIT.

       LABEL1 SECTION.
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
                      display "PGM LEAV9POW: I AM LABEL1 CALL N " I
                      PERFORM LABEL2 THRU E--LABEL2
                      PERFORM LABEL3 THRU E--LABEL3
               END-PERFORM
               CONTINUE.
       E--LABEL1 SECTION.
               DISPLAY "EXIT SECTION".

       LABEL2.
                     display "PGM LEAV9POW: I AM LABEL2"
                     display "PGM LEAV9POW: I WILL LEAVE LABEL1"
                     IF J = 1 THEN
                            DISPLAY "EXIT SECTION"
                     END-IF
                     CONTINUE.
       E--LABEL2.
               EXIT.

       LABEL3.
               display "PGM LEAV9POW: I AM LABEL3"
               CONTINUE.
       E--LABEL3.
               EXIT.

       LABEL4.
               display "PGM LEAV9POW: I AM LABEL4"
               CONTINUE.
       E--LABEL4.
               EXIT.
       END PROGRAM EXITSEC0.
	   
