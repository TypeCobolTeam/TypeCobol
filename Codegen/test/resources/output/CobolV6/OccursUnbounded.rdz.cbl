Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OccursTest.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 arrays.
         05 data1 PIC X OCCURS UNBOUNDED.
         05 data2 PIC X OCCURS 1 TO UNBOUNDED.
       01 group1.
         05 var1 PIC 9(18).
         05 array PIC X OCCURS 1 TO UNBOUNDED DEPENDING ON var1.

       PROCEDURE DIVISION.
       .

       END PROGRAM OccursTest.
