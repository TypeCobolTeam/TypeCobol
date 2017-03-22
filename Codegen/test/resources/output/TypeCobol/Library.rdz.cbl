       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZDATE.
       AUTHOR. REYDELPA.
       
      *=================================================================
       ENVIRONMENT DIVISION.
      *=================================================================
       CONFIGURATION SECTION.
      *_________________________________________________________________
      *SOURCE-COMPUTER.    IBM-3033 WITH DEBUGGING MODE.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
      *=================================================================
       DATA DIVISION.
      *=================================================================
       WORKING-STORAGE SECTION.
       77  C-WSS                     PIC X(03) VALUE 'WSS'.
       01  W-IfrPgm.
           05 C-PgmNme               PIC X(08) Value 'LIBDATE'.

      *01  dateJulian    TYPEDEF strict.
      *    10 YYYY                   PIC 9(04).
      *    10 DDD                    PIC 9(03).

      *01  dateDB2       TYPEDEF strict.
      *    10 YYYY                   PIC 9(04).
      *    10                        PIC X(01).
      *    10 MM                     PIC 9(02).
      *    10                        PIC X(01).
      *    10 DD                     PIC 9(02).

      *01  dateString    TYPEDEF strict  PIC 9(08).

      *01 culture        TYPEDEF strict.
      *    10 lng                    PIC X(02).
      *    10 cty                    PIC X(02).
       01  TC-DVZZDATE-FctList-Loaded PIC X(02) EXTERNAL.
           88 TC-DVZZDATE-FctList-IsLoaded      VALUE 'OK'.
      *DVZZDATE::currentDate
        01 TC-DVZZDATE-e5f209fa PROCEDURE-POINTER EXTERNAL.
      *DVZZDATE::currentDateDB2
        01 TC-DVZZDATE-b8ac0397 PROCEDURE-POINTER EXTERNAL.
      *DVZZDATE::currentDateJulian
        01 TC-DVZZDATE-c4e76b45 PROCEDURE-POINTER EXTERNAL.
      *DVZZDATE::currentDateFreeFormat
        01 TC-DVZZDATE-d55b3ea7 PROCEDURE-POINTER EXTERNAL.
      *DVZZDATE::currentDateString
        01 TC-DVZZDATE-bfb0fa9b PROCEDURE-POINTER EXTERNAL.


      *=================================================================
       PROCEDURE DIVISION.
      *
      *    IF CallIsCopy
      *      PERFORM Copy-Process-Mode
      *    ELSE
           PERFORM FctList-Process-Mode
      *    END-IF

           GOBACK.

        FctList-Process-Mode.
            IF NOT TC-DVZZDATE-FctList-IsLoaded
              SET TC-DVZZDATE-e5f209fa   TO ENTRY 'e5f209fa'
              SET TC-DVZZDATE-b8ac0397   TO ENTRY 'b8ac0397'
              SET TC-DVZZDATE-c4e76b45   TO ENTRY 'c4e76b45'
              SET TC-DVZZDATE-d55b3ea7   TO ENTRY 'd55b3ea7'
              SET TC-DVZZDATE-bfb0fa9b   TO ENTRY 'bfb0fa9b'

              SET TC-DVZZDATE-FctList-IsLoaded TO TRUE
            END-IF
               .
           .
                          
      *=================================================================
      *DECLARE FUNCTION currentDate PUBLIC
      *Description of currentDate
      *    RETURNING Result TYPE date.
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateDB2 PUBLIC
      *    RETURNING Result Type dateDB2.
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateJulian PUBLIC
      * my comment
      *    RETURNING Result Type dateJulian.
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateFreeFormat PUBLIC
      *                   INPUT dateType   PIC X(01)
      *                         direction  PIC X(01)
      *                         separator  PIC X(01)
      *                         culture    TYPE culture
      *                         returnCode PIC 9(04)
      *                   RETURNING Result PIC X(40).
      *_________________________________________________________________
      *Keep spaces at end of line, because there were 
      * presents in source file                      
      *DECLARE FUNCTION currentDateString PUBLIC
      *    RETURNING Result TYPE dateString.

       END PROGRAM DVZZDAT.
      *
      *DECLARE FUNCTION currentDate PUBLIC
      *Description of currentDate
      *    RETURNING Result TYPE date.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. e5f209fa.
       DATA DIVISION.
       LINKAGE SECTION.
       01 Result.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END PROGRAM e5f209fa.
      *
      *DECLARE FUNCTION currentDateDB2 PUBLIC
      *    RETURNING Result Type dateDB2.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. b8ac0397.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01  W-Dat       TYPE date.
       01 W-Dat.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                 
       LINKAGE SECTION.
       01 Result.
           02 YYYY PIC 9(04).
           02 PIC X(01).
           02 MM PIC 9(02).
           02 PIC X(01).
           02 DD PIC 9(02).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .


           ACCEPT W-Dat             FROM DATE YYYYMMDD
      *    move W-Dat :: YYYY       to Result :: YYYY
           move YYYY  OF  W-Dat       to YYYY  OF  Result
      *    move W-Dat :: MM         to Result :: MM
           move MM  OF  W-Dat         to MM  OF  Result
      *    move W-Dat :: DD         to Result :: DD
           move DD  OF  W-Dat         to DD  OF  Result 
      *    move '-'                 to Result(5:1)
      *    move '-'                 to Result(8:1)
           
           .
       END PROGRAM b8ac0397.
      *
      *DECLARE FUNCTION currentDateJulian PUBLIC
      * my comment
      *    RETURNING Result Type dateJulian.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. c4e76b45.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01  W-Dat       TYPE date.
       01 W-Dat.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                 
       LINKAGE SECTION.
       01 Result.
           02 YYYY PIC 9(04).
           02 DDD PIC 9(03).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .


           ACCEPT W-Dat             FROM DATE YYYYMMDD
      *    move unsafe W-Dat to Result
           move        W-Dat to Result
                                      
           .
       END PROGRAM c4e76b45.
      *
      *DECLARE FUNCTION currentDateFreeFormat PUBLIC
      *                   INPUT dateType   PIC X(01)
      *                         direction  PIC X(01)
      *                         separator  PIC X(01)
      *                         culture    TYPE culture
      *                         returnCode PIC 9(04)
      *                   RETURNING Result PIC X(40).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. d55b3ea7.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01  W-Dat       TYPE date.
       01 W-Dat.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                 
       LINKAGE SECTION.
       01 dateType PIC X(01).
       01 direction PIC X(01).
       01 separator PIC X(01).
       01 culture.
           02 lng PIC X(02).
           02 cty PIC X(02).
       01 returnCode PIC 9(04).
       01 Result PIC X(40).
       PROCEDURE DIVISION
             USING BY REFERENCE dateType
                   BY REFERENCE direction
                   BY REFERENCE separator
                   BY REFERENCE culture
                   BY REFERENCE returnCode
                   BY REFERENCE Result
           .


           continue
           .
       END PROGRAM d55b3ea7.
      *
      *DECLARE FUNCTION currentDateString PUBLIC
      *    RETURNING Result TYPE dateString.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bfb0fa9b.
       DATA DIVISION.
       LINKAGE SECTION.
       01 Result PIC 9(08).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END PROGRAM bfb0fa9b.
