Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
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

      *01  dateJulian    TYPEDEF strict PUBLIC.
      *    10 YYYY                   PIC 9(04).
      *    10 DDD                    PIC 9(03).

      *01  dateDB2       TYPEDEF strict PUBLIC.
      *    10 YYYY                   PIC 9(04).
      *    10                        PIC X(01).
      *    10 MM                     PIC 9(02).
      *    10                        PIC X(01).
      *    10 DD                     PIC 9(02).

      *01  dateString    TYPEDEF strict PUBLIC PIC 9(08).

      *01 culture        TYPEDEF strict PUBLIC.
      *    10 lng                    PIC X(02).
      *    10 cty                    PIC X(02).

       LINKAGE SECTION.
       01 TC-FunctionCode pic X(30).
      * Function which call program db42865c
      * Which is generated code for DVZZDATE.currentDate
           88 Fct-db42865c-currentDate
              value 'Fct=db42865c-currentDate'.
      * Function which call program fa5ee5e9
      * Which is generated code for DVZZDATE.currentDateDB2
           88 Fct-fa5ee5e9-currentDateDB2
              value 'Fct=fa5ee5e9-currentDateDB2'.
      * Function which call program cf63e86f
      * Which is generated code for DVZZDATE.currentDateJulian
           88 Fct-cf63e86f-currentDateJulian
              value 'Fct=cf63e86f-currentDateJulian'.
      * Function which call program cfc290ce
      * Which is generated code for DVZZDATE.currentDateFreeFormat
           88 Fct-cfc290ce-currentDateFreeFormat
              value 'Fct=cfc290ce-currentDateFreeFormat'.
      * Function which call program b8721d20
      * Which is generated code for DVZZDATE.currentDateString
           88 Fct-b8721d20-currentDateString
              value 'Fct=b8721d20-currentDateString'.

       01 arg1 pic X.
       01 arg2 pic X.
       01 arg3 pic X.
       01 arg4 pic X.
       01 arg5 pic X.

      *=================================================================
       PROCEDURE DIVISION USING TC-FunctionCode
                          arg1
                          arg2
                          arg3
                          arg4
                          arg5.
           PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

       FctList-Process-Mode.
           evaluate true
              when Fct-db42865c-currentDate
                 call 'db42865c'
              when Fct-fa5ee5e9-currentDateDB2
                 call 'fa5ee5e9'
              when Fct-cf63e86f-currentDateJulian
                 call 'cf63e86f'
              when Fct-cfc290ce-currentDateFreeFormat
                 call 'cfc290ce' using arg1
                                        arg2
                                        arg3
                                        arg4
                                        arg5
              when Fct-b8721d20-currentDateString
                 call 'b8721d20'
              when other
                 Perform Handle-Error
           end-evaluate
           .
       Handle-Error.
           continue
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
      *
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
       PROGRAM-ID. db42865c.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       LINKAGE SECTION.
      *DVZZDATE.currentDate  - No Params
      *     returns(Result: DATE)
       01 Result.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .
      *DVZZDATE.currentDate  - No Params
      *     returns(Result: DATE)
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END PROGRAM db42865c.


      *
      *DECLARE FUNCTION currentDateDB2 PUBLIC
      *    RETURNING Result Type dateDB2.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. fa5ee5e9.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *DVZZDATE.currentDateDB2  - No Params
      *     returns(Result: dateDB2)
                               
      *01  W-Dat       TYPE date.
       01 W-Dat.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                 

       LINKAGE SECTION.
      *DVZZDATE.currentDateDB2  - No Params
      *     returns(Result: dateDB2)
       01 Result.
           02 YYYY PIC 9(04).
           02 PIC X(01).
           02 MM PIC 9(02).
           02 PIC X(01).
           02 DD PIC 9(02).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .
      *DVZZDATE.currentDateDB2  - No Params
      *     returns(Result: dateDB2)

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
       END PROGRAM fa5ee5e9.


      *
      *DECLARE FUNCTION currentDateJulian PUBLIC
      * my comment
      *    RETURNING Result Type dateJulian.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cf63e86f.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *DVZZDATE.currentDateJulian  - No Params
      *     returns(Result: dateJulian)
                               
      *01  W-Dat       TYPE date.
       01 W-Dat.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                 

       LINKAGE SECTION.
      *DVZZDATE.currentDateJulian  - No Params
      *     returns(Result: dateJulian)
       01 Result.
           02 YYYY PIC 9(04).
           02 DDD PIC 9(03).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .
      *DVZZDATE.currentDateJulian  - No Params
      *     returns(Result: dateJulian)

           ACCEPT W-Dat             FROM DATE YYYYMMDD
      *    move unsafe W-Dat to Result
           move        W-Dat to Result
           .
       END PROGRAM cf63e86f.


      *
      *DECLARE FUNCTION currentDateFreeFormat PUBLIC
      *                   INPUT dateType   PIC X(01)
      *                         direction  PIC X(01)
      *                         separator  PIC X(01)
      *
      *                         culture    TYPE culture
      *                         returnCode PIC 9(04)
      *                   RETURNING Result PIC X(40).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cfc290ce.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *DVZZDATE.currentDateFreeFormat - Params :
      *     input(dateType: pic X(01), direction: pic X(01), separator:
      *pic X(01), culture: culture, returnCode: pic 9(04))
      *     returns(Result: pic X(40))
                               
      *01  W-Dat       TYPE date.
       01 W-Dat.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                 

       LINKAGE SECTION.
      *DVZZDATE.currentDateFreeFormat - Params :
      *     input(dateType: pic X(01), direction: pic X(01), separator:
      *pic X(01), culture: culture, returnCode: pic 9(04))
      *     returns(Result: pic X(40))
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
      *DVZZDATE.currentDateFreeFormat - Params :
      *     input(dateType: pic X(01), direction: pic X(01), separator:
      *pic X(01), culture: culture, returnCode: pic 9(04))
      *     returns(Result: pic X(40))

           continue
           .
       END PROGRAM cfc290ce.


      *
      *DECLARE FUNCTION currentDateString PUBLIC
      *    RETURNING Result TYPE dateString.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. b8721d20.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       LINKAGE SECTION.
      *DVZZDATE.currentDateString  - No Params
      *     returns(Result: dateString)
       01 Result PIC 9(08).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .
      *DVZZDATE.currentDateString  - No Params
      *     returns(Result: dateString)
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END PROGRAM b8721d20.


