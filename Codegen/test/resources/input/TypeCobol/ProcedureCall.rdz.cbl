﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcedureCall.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.

       01  somedate     TYPE Date.
       01 myDate2       type Date.
       01 myDate3       type Date.
       01 myDate4       type Date.
       01 myPointer     pointer.

       01  someformat   PIC X(08).
       01  flag         TYPE Bool.
       01  realformat   PIC X(08).

       01 funcPointer function-pointer.
       01 procPointer procedure-pointer.

       PROCEDURE DIVISION.

       DECLARE PROCEDURE GetPtrFn
           INPUT  ptrFn function-pointer
                  ptrPr procedure-pointer.
       PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       DECLARE PROCEDURE ProcedureWithANameOver22Chars PRIVATE.
       END-DECLARE.

       DECLARE PROCEDURE ValidateDateFormat PRIVATE.
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
                 arg0          pointer
          IN-OUT myDate3       type Date
                 myDate4       type Date
          OUTPUT okay          TYPE Bool
                 actual-format PIC X(08).
         .
       PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.

       TRAITEMENT.

           CALL GetPtrFn 
             INPUT funcPointer procPointer.

           CALL ProcedureWithANameOver22Chars.

      * __________________________________________________
      * OK : proper parameter list (TCRFUN_CALL_PARAMETER_ORDER)
           CALL ValidateDateFormat
                    INPUT      somedate someformat
                    OUTPUT     flag     realformat
       
           CALL ValidateDateFormat
                    INPUT      somedate by content 'YYYYMMDD'
                    OUTPUT     flag     realformat
           END-CALL
      * __________________________________________________
      * OK : parameter number for a procedure
      *      however, this is parsed as a standard COBOL call
      *    Will change after issue #366
           CALL ValidateDateFormat END-CALL
      * __________________________________________________
      * OK with INPUT on the same line as call
           CALL ValidateDateFormat INPUT      somedate 
                                              by content 'YYYYMMDD'
                                   OUTPUT     flag     realformat
           END-CALL
           .      
      * __________________________________________________
      * OK  by content
           CALL ValidateDateFormat INPUT     by content somedate 
                                               'YYYYMMDD'
                                   OUTPUT     flag     realformat
           END-CALL
           .      
      * __________________________________________________
      * OK  
           CALL myProc  INPUT     by content somedate 
                                               'YYYYMMDD'
                                             myDate2
                                  address of myDate2
                        IN-OUT myDate3 myDate4
                                   OUTPUT     flag     realformat
           END-CALL     
      * __________________________________________________
      * OK  
           CALL myProc  INPUT  somedate 
                               by content 'YYYYMMDD'
                               by reference myDate2
                               by content   address of myDate2
                        IN-OUT myDate3
                               myDate4
                        OUTPUT flag     
                               realformat
           END-CALL
      * __________________________________________________
      * OK  
           CALL myProc  INPUT  somedate 
                               by content 'YYYYMMDD'
                               by reference myDate2
                               myPointer
                        IN-OUT myDate3
                               myDate4
                        OUTPUT flag     
                               realformat
           END-CALL
           .

       END PROGRAM ProcedureCall.
