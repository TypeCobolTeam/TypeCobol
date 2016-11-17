       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcedureCall.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.

       01  somedate     TYPE Date.
       01  someformat   PIC X(08).
       01  flag         TYPE Bool.
       01  realformat   PIC X(08).

       PROCEDURE DIVISION.
       
       DECLARE PROCEDURE ValidateDateFormat PRIVATE
           INPUT mydate        TYPE Date
                 format        PIC X(08)
          OUTPUT okay          TYPE Bool
                 actual-format PIC X(08).
         .
         PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       TRAITEMENT.
      * __________________________________________________
      * OK : proper parameter list (TCRFUN_CALL_PARAMETER_ORDER)
           CALL ValidateDateFormat
                    INPUT      somedate someformat
                    OUTPUT     flag     realformat
       
           CALL ValidateDateFormat
                    INPUT      somedate 'YYYYMMDD'
                    OUTPUT     flag     realformat
           END-CALL
      * __________________________________________________
      * KO : procedure doesn't exist
           CALL ValidateDatFormatt
                    INPUT      somedate someformat
                    OUTPUT              realformat
           END-CALL
      * __________________________________________________
      * OK : parameter number for a procedure
      *      however, this is parsed as a standard COBOL call
           CALL ValidateDateFormat END-CALL
      * __________________________________________________
      * KO : wrong parameter number (TCRFUN_MATCH_PARAMETERS_NUMBER)
           CALL ValidateDateFormat
                    INPUT      somedate someformat
                    OUTPUT              realformat
           END-CALL
      * __________________________________________________
      * KO : wrong parameter order (TCRFUN_MATCH_PARAMETERS_TYPE)
           CALL ValidateDateFormat
                    INPUT      someformat somedate
                    OUTPUT     realformat flag
           .

       END PROGRAM ProcedureCall.
