       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT1.
       DATA DIVISION.
       PROCEDURE DIVISION.
       main.
           PERFORM a
           GOBACK
           .
       a.
           PERFORM b
           PERFORM c
           .
       b.
           PERFORM d
           .
       c.
           PERFORM d
           .
       d.
           DISPLAY "end !"
           .
       END PROGRAM DVZZMFT1.