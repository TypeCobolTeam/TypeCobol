       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH02.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STUDENTS.
          02 NAMES OCCURS 10 ASCENDING KEY IS STUDENT-NAME
                             INDEXED BY STD-IDX.
             03 STUDENT-NAME PIC X(30).
          02 SUBJECT PIC 9(3) OCCURS 6
                              INDEXED BY SEQ.
       01 IN-STD-NAME PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
           PERFORM SMPL-SEARCHALL
           GOBACK
           .
       SMPL-SEARCHALL.
           DISPLAY 'SEARCH ALL EXAMPLE....'
           SET SEQ TO 1
           SEARCH ALL NAMES
              AT END
                 PERFORM NOT-FOUND
              WHEN STUDENT-NAME (STD-IDX) = IN-STD-NAME
                 PERFORM FOUND
           END-SEARCH
           .
       NOT-FOUND.
           DISPLAY "STUDENT DETAILS NOT FOUND"
           .
       FOUND.
           DISPLAY "STUDENT DETAILS FOUND"
           .
       END PROGRAM SEARCH02.