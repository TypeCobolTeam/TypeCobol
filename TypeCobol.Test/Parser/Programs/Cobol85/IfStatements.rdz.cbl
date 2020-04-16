       IDENTIFICATION DIVISION.
       PROGRAM-ID. IfStatements.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9.
       01 B PIC 9.
       PROCEDURE DIVISION.
           IF A > B .
           IF A > B ELSE .
           IF A > B ELSE END-IF.
           IF A > B ELSE DISPLAY B .
           IF A > B ELSE DISPLAY B END-IF.
           IF A > B ELSE NEXT SENTENCE .
           IF A > B ELSE NEXT SENTENCE END-IF.
           IF A > B END-IF.

           IF A > B DISPLAY A .
           IF A > B DISPLAY A ELSE .
           IF A > B DISPLAY A ELSE END-IF.
           IF A > B DISPLAY A ELSE DISPLAY B .
           IF A > B DISPLAY A ELSE DISPLAY B END-IF.
           IF A > B DISPLAY A ELSE NEXT SENTENCE .
           IF A > B DISPLAY A ELSE NEXT SENTENCE END-IF.
           IF A > B DISPLAY A END-IF.

           IF A > B NEXT SENTENCE .
           IF A > B NEXT SENTENCE ELSE .
           IF A > B NEXT SENTENCE ELSE END-IF.
           IF A > B NEXT SENTENCE ELSE DISPLAY B .
           IF A > B NEXT SENTENCE ELSE DISPLAY B END-IF.
           IF A > B NEXT SENTENCE ELSE NEXT SENTENCE .
           IF A > B NEXT SENTENCE ELSE NEXT SENTENCE END-IF.
           IF A > B NEXT SENTENCE END-IF.
           .
       END PROGRAM IfStatements.