       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       PROCEDURE DIVISION.
           PERFORM B  THRU B
           PERFORM S2 THRU S2
           PERFORM S1 THRU C
           PERFORM B  THRU S2
           PERFORM S0 THRU S1
           PERFORM D  THRU E
           PERFORM D  THRU S3
           GOBACK.
       S0 SECTION.
           display 'no paragraphs in this section'
           .
       S1 SECTION.
       A.
           display 'A'
           .
       B.
           display 'B'
           .
       S2 SECTION.
       C.
           display 'C'
           .
       D.
           display 'D'
           .
       S3 SECTION.
           display 'in section S3'
           .
       E.
           display 'E'
           .
       END PROGRAM DVZZMFT3.