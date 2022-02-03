000000 IDENTIFICATION DIVISION.
000000* Test to check if COPY are not expanded when code is generated
000000 PROGRAM-ID.   CopyNotExpanded.
      *REMARKS. COPY=(
      *        CopyData
      *        CopyProcedure
      *        ).
                                                                                                                                             
000000
000000 DATA DIVISION.
000000 WORKING-STORAGE SECTION.
000000 01 identifier-4 PIC 9(04).
000000 COPY CopyData.
000000 01 identifier-5 PIC 9(04).
000000
000000 PROCEDURE DIVISION.
000000     display "before copy"
000000     COPY CopyProcedure.
000000     display "after copy"
000000     .
000000
000000 END PROGRAM CopyNotExpanded.
