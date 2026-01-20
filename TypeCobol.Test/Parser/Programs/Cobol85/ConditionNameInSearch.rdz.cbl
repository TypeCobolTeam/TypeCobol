      * Commentaire
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHKCDNNM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MAIN-GROUP.
          05 VAR1      PIC X.
             88 COND1        VALUE 'A'.
             88 COND2        VALUE 'B'.
          05 VAR2      PIC 9.
       01 GROUP-SEARCH.
         02 ELEM OCCURS 12 INDEXED BY IDX.
           05 NUM     PIC 99.
           05 NBJ     PIC 99.
           05 LIB    PIC X(9).
       01 STUDENTS.
          02 NAMES OCCURS 10 ASCENDING KEY IS STUDENT-STATUS
                             ASCENDING KEY IS STUDENT-CAT
                             INDEXED BY STD-IDX.
             03 STUDENT-NAME PIC X(30).
             03 STUDENT-STATUS PIC X.
                88 STATUS1        VALUE 'A'.
                88 STATUS2        VALUE 'B'.
             03 STUDENT-CAT    PIC X(5).
                88 CAT1        VALUE 'CAT-A'.
                88 CAT2        VALUE 'CAT-B'.

       PROCEDURE DIVISION.

           SET IDX TO 1.
           SET STD-IDX TO 1.

      * KO
           SEARCH ELEM
              WHEN VAR1
                 DISPLAY NUM(IDX) "/" LIB(IDX)
              WHEN (COND1) AND (VAR1 = 'A') AND (VAR2)
                 NEXT SENTENCE.

      * OK
           SEARCH ELEM
              WHEN VAR1 = 'A'
                 DISPLAY NUM(IDX) "/" LIB(IDX)
              WHEN (COND1) AND (VAR1 = 'A') AND (COND2)
                 NEXT SENTENCE.

      * KO
           SEARCH ALL NAMES
              AT END
                 PERFORM NOT-FOUND
              WHEN STUDENT-CAT(STD-IDX)
                AND STUDENT-STATUS(STD-IDX)
                 PERFORM FOUND
           END-SEARCH

      * OK
           SEARCH ALL NAMES
              AT END
                 PERFORM NOT-FOUND
              WHEN CAT1(STD-IDX)
                AND STATUS1(STD-IDX)
                 PERFORM FOUND
           END-SEARCH

      * KO
           SEARCH ALL NAMES
              AT END
                 PERFORM NOT-FOUND
              WHEN VAR-NOT-DEFINED
                 PERFORM FOUND
           END-SEARCH

           GOBACK
           .

       NOT-FOUND.
           DISPLAY 'in NOT-FOUND'
           .

       FOUND.
           DISPLAY 'in FOUND'
           .

       END PROGRAM CHKCDNNM.