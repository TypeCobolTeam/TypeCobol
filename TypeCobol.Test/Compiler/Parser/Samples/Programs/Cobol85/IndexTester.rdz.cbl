       IDENTIFICATION DIVISION.
       PROGRAM-ID. IndexTester.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ZONE PIC X(5).
       01 TB-1.
           05 EL-1 PIC X(5) OCCURS 10 INDEXED BY IND-1.

       01 ZONEBIS PIC X(5).
       01 TB-2.
           05 EL-2 PIC X(5) OCCURS 10 INDEXED BY IND-2, IND-3.

       PROCEDURE DIVISION.
      * OK : Test with 1 index
           MOVE EL-1(IND-1) TO ZONE  
           SET IND-1 TO 1

      * OK : Test with 2 Indexes    
           MOVE EL-2(IND-2) TO ZONEBIS  
           SET IND-2 TO 1
           SET IND-3 TO 1
           . 

       END PROGRAM IndexTester.