﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcedureCall.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.

      *01  somedate     TYPE Date.
       01 somedate.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                  
      *01 myDate2       type Date.
       01 myDate2.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                  
      *01 myDate3       type Date.
       01 myDate3.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                  
      *01 myDate4       type Date.
       01 myDate4.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                  

       01  someformat   PIC X(08).
      *01  flag         TYPE Bool.
       01  flag-value PIC X VALUE LOW-VALUE.
           88  flag       VALUE 'T'.
           88  flag-false VALUE 'F'.
                                  
       01  realformat   PIC X(08).

       PROCEDURE DIVISION.

      *DECLARE PROCEDURE ValidateDateFormat PRIVATE.

      *DECLARE PROCEDURE ValidateDateFormat PRIVATE
      *    INPUT mydate        TYPE Date
      *          format        PIC X(08)
      *   OUTPUT okay          TYPE Bool
      *          actual-format PIC X(08).
      *  .

      *DECLARE PROCEDURE myProc PRIVATE
      *   INPUT  mydate        TYPE Date
      *          format        PIC X(08)
      *          myDate2       type Date
      *   IN-OUT myDate3       type Date
      *          myDate4       type Date
      *   OUTPUT okay          TYPE Bool
      *          actual-format PIC X(08).
      *  .

       TRAITEMENT.
      * __________________________________________________
      * OK : proper parameter list (TCRFUN_CALL_PARAMETER_ORDER)
      *    CALL ValidateDateFormat
      *             INPUT      somedate someformat
      *             OUTPUT     flag     realformat
           CALL 'c5875eec' USING 
                                 somedate
                                 someformat
                    by reference flag-value
                                 realformat
           end-call
                                                  
       
      *    CALL ValidateDateFormat
      *             INPUT      somedate by content 'YYYYMMDD'
      *             OUTPUT     flag     realformat
           CALL 'c5875eec' USING 
                                 somedate
                    by content   'YYYYMMDD'
                    by reference flag-value
                                 realformat
                                                  
           END-CALL
      * __________________________________________________
      * OK : parameter number for a procedure
      *      however, this is parsed as a standard COBOL call
      *    Will change after issue #366
      *    CALL ValidateDateFormat END-CALL
           CALL 'd5130bbc' 
                                   END-CALL
      * __________________________________________________
      * OK with INPUT on the same line as call
      *    CALL ValidateDateFormat INPUT      somedate
      *                                       by content 'YYYYMMDD'
      *                            OUTPUT     flag     realformat
           CALL 'c5875eec' USING 
                                 somedate
                    by content   'YYYYMMDD'
                    by reference flag-value
                                 realformat
                                                                 
           END-CALL
           .      
      * __________________________________________________
      * OK  by content
      *    CALL ValidateDateFormat INPUT     by content somedate
      *                                        'YYYYMMDD'
      *                            OUTPUT     flag     realformat
           CALL 'c5875eec' USING 
                    by content   somedate
                                 'YYYYMMDD'
                    by reference flag-value
                                 realformat
                                                                 
           END-CALL
           .      
      * __________________________________________________
      * OK  
      *    CALL myProc  INPUT     by content somedate
      *                                        'YYYYMMDD'
      *                                      myDate2
      *                 IN-OUT myDate3 myDate4
      *                            OUTPUT     flag     realformat
           CALL 'd5ec4efc' USING 
                    by content   somedate
                                 'YYYYMMDD'
                                 myDate2
                    by reference myDate3
                                 myDate4
                    by reference flag-value
                                 realformat
                                                                 
           END-CALL     
      * __________________________________________________
      * OK  
      *    CALL myProc  INPUT  somedate
      *                        by content 'YYYYMMDD'
      *                        by reference myDate2
      *                 IN-OUT myDate3
      *                        myDate4
      *                 OUTPUT flag
      *                        realformat
           CALL 'd5ec4efc' USING 
                                 somedate
                    by content   'YYYYMMDD'
                    by reference myDate2
                    by reference myDate3
                                 myDate4
                    by reference flag-value
                                 realformat
                                         
           END-CALL
           .

       END PROGRAM ProcedureCall.
      *
      *DECLARE PROCEDURE ValidateDateFormat PRIVATE.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. d5130bbc.
       END PROGRAM d5130bbc.
      *
      *DECLARE PROCEDURE ValidateDateFormat PRIVATE
      *    INPUT mydate        TYPE Date
      *          format        PIC X(08)
      *   OUTPUT okay          TYPE Bool
      *          actual-format PIC X(08).
      *  .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. c5875eec.
       DATA DIVISION.
       LINKAGE SECTION.
       01 mydate.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       01 format PIC X(08).
       01 okay-value PIC X     VALUE LOW-VALUE.
           88 okay       VALUE 'T'.
           88 okay-false VALUE 'F'.
       01 actual-format PIC X(08).
       PROCEDURE DIVISION
             USING BY REFERENCE mydate
                   BY REFERENCE format
                   BY REFERENCE okay-value
                   BY REFERENCE actual-format
           .
           CONTINUE.
       END PROGRAM c5875eec.
      *
      *DECLARE PROCEDURE myProc PRIVATE
      *   INPUT  mydate        TYPE Date
      *          format        PIC X(08)
      *          myDate2       type Date
      *   IN-OUT myDate3       type Date
      *          myDate4       type Date
      *   OUTPUT okay          TYPE Bool
      *          actual-format PIC X(08).
      *  .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. d5ec4efc.
       DATA DIVISION.
       LINKAGE SECTION.
       01 mydate.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       01 format PIC X(08).
       01 myDate2.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       01 myDate3.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       01 myDate4.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       01 okay-value PIC X     VALUE LOW-VALUE.
           88 okay       VALUE 'T'.
           88 okay-false VALUE 'F'.
       01 actual-format PIC X(08).
       PROCEDURE DIVISION
             USING BY REFERENCE mydate
                   BY REFERENCE format
                   BY REFERENCE myDate2
                   BY REFERENCE myDate3
                   BY REFERENCE myDate4
                   BY REFERENCE okay-value
                   BY REFERENCE actual-format
           .
           CONTINUE.
       END PROGRAM d5ec4efc.
