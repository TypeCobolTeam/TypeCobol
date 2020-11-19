       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT1.
       PROCEDURE DIVISION.
           PERFORM A THRU B
      *Wrong order between two paragraphs
           PERFORM B THRU A
           PERFORM S1 THRU S2
      *Wrong order between two sections
           PERFORM S2 THRU S1
      *Wrong order between a paragraph and a section
           PERFORM S1 THRU A
           GOBACK.
       A.
           display "A"
           .
       B.
           display "B"
           .
       S1 SECTION.
           display "S1"
           .
       S2 SECTION.
           display "S2"
           .
       END PROGRAM DVZZMFT1.