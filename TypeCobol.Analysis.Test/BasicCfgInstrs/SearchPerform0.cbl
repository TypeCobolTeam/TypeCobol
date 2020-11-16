       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STUDENTS.
          02 NAMES OCCURS 10 ASCENDING KEY IS STUDENT-NAME
                             INDEXED BY STD-IDX.
             03 STUDENT-NAME PIC X(30).
          02 SUBJECT PIC 9(3) OCCURS 6
                              INDEXED BY SEQ.
       PROCEDURE DIVISION.
           PERFORM SMPL-SEARCH
           GOBACK
           .
       SMPL-SEARCH.
           DISPLAY 'SEARCH EXAMPLE....'
           SET SEQ TO 1
           SEARCH SUBJECT
              AT END
                 PERFORM PASSED
              WHEN SUBJECT(SEQ) < 10
                 PERFORM FAILED
           END-SEARCH
           .
       PASSED.
           DISPLAY "STUDENT PASSED"
           .
       FAILED.
           DISPLAY "STUDENT FAILED"
           .
       END PROGRAM SEARCH01.