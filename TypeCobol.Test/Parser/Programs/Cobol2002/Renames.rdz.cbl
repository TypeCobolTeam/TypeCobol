       IDENTIFICATION DIVISION.
       PROGRAM-ID. RenamesPgm.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 var1.
         05 D1 PIC X(10).
       66 var2 renames D1.
       66 var3 renames var1.

       PROCEDURE DIVISION.
           .

       END PROGRAM RenamesPgm.