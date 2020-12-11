       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       special-names.
           decimal-point comma
           SYMBOLIC CHARACTERS LOW-VAL IS 1
                               HEX-15  IS 22
                               HI-VAL  IS 256
           CLASS NAMECHAR IS 'A' THRU 'I'
                             'J' THRU 'R'
                             'S' THRU 'Z'
           .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC X(10) VALUE LOW-VAL.
       01 var2 PIC X(10) VALUE HEX-15.
       01 var3 PIC X(10) VALUE HI-VAL.
       01 t1 PIC X(10).
       01 t2 PIC X(10).
       01 t3 PIC X(10).
       01 c PIC X.
       PROCEDURE DIVISION.
           MOVE 'M' TO c
           COPY CopyUsingSpecialNames.
           GOBACK
           .
       END PROGRAM TCOMFL06.