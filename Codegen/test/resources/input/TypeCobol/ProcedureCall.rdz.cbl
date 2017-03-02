       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcedureCall.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.

       01  somedate     TYPE Date.
       01 myDate2       type Date.
       01 myDate3       type Date.
       01 myDate4       type Date.

       01  someformat   PIC X(08).
       01  flag         TYPE Bool.
       01  realformat   PIC X(08).

       PROCEDURE DIVISION.

       DECLARE PROCEDURE ValidateDateFormat PRIVATE.
         PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       DECLARE PROCEDURE ValidateDateFormat PRIVATE
           INPUT mydate        TYPE Date
                 format        PIC X(08)
          OUTPUT okay          TYPE Bool
                 actual-format PIC X(08).
         .
       PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       DECLARE PROCEDURE myProc PRIVATE
          INPUT  mydate        TYPE Date
                 format        PIC X(08)
                 myDate2       type Date
          IN-OUT myDate3       type Date
                 myDate4       type Date
          OUTPUT okay          TYPE Bool
                 actual-format PIC X(08).
         .
       PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       TRAITEMENT.
     
      * __________________________________________________
      * OK : parameter number for a procedure
      *      however, this is parsed as a standard COBOL call
      *    Will change after issue #366
           CALL ValidateDateFormat
           .

       END PROGRAM ProcedureCall.
