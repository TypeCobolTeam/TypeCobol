       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01   X.
            05  X1   PIC X.
            05  X2   PIC X.
            05  X3   PIC X.
            05  X4   PIC X.
            05  X5   PIC X.
       66   Y RENAMES X2 THRU X4.
       PROCEDURE DIVISION.
           MOVE 'ABCD' TO Y
           GOBACK
           .
       END PROGRAM PGM.