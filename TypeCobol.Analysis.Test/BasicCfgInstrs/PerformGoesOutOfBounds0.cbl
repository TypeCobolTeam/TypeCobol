       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT1.
       PROCEDURE DIVISION.
           perform a
           goback
           .
       a.
           display 'a'
      *Exiting paragragh with a GO TO
           go to b
           .
       b.
           display 'b'
           .
       END PROGRAM DVZZMFT1.
