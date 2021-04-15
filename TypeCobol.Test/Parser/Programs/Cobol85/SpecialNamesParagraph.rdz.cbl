       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       special-names.
           decimal-point comma
      
           ALPHABET alphabet1 IS STANDARD-1
           ALPHABET alphabet2 IS STANDARD-2
           ALPHABET alphabet3 IS NATIVE
           ALPHABET alphabet4 IS EBCDIC
           ALPHABET alphabet5 IS SPACE
           ALPHABET alphabet6 IS LOW-VALUE THROUGH HIGH-VALUE
           ALPHABET alphabet7 IS ZERO THRU QUOTE
           ALPHABET alphabet8 IS SPACE ALSO QUOTE
           ALPHABET alphabet9 IS SPACE ALSO QUOTE ALSO ZERO
      
           SYMBOLIC CHARACTERS LOW-VAL IS 1
                               HEX-15  IS 22
                               HI-VAL  IS 256
      
           CLASS NAMECHAR IS 'A' THRU 'I'
                             'J' THRU 'R'
                             'S' THRU 'Z'
           CLASS class1 IS LOW-VALUE THROUGH HIGH-VALUE
           CLASS class2 IS SPACE
           CLASS class3 QUOTE
           CLASS class4 IS ZERO
           CLASS class5 IS LOW-VALUE QUOTE SPACE HIGH-VALUE ZERO
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