       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcedureCall.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.

       01  somedate     TYPE Date.
       01  someformat   PIC X(08).
       01  flag         TYPE Bool.
       01  realformat   PIC X(08).

       PROCEDURE DIVISION.
       
       DECLARE FUNCTION ValidateDateFormat PRIVATE
           INPUT mydate        TYPE Date
                 format        PIC X(08)
          OUTPUT okay          TYPE Bool
                 actual-format PIC X(08).
         .
         PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       TRAITEMENT.
           CALL ValidateDateFormat
                    INPUT      somedate someformat
                    OUTPUT     flag     realformat
           END-CALL
           .

       END PROGRAM ProcedureCall.
