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
       01  TC-DVZZDATE-FctList-Loaded PIC X(02).
           88 TC-DVZZDATE-FctList-IsLoaded      VALUE 'OK'.
       01 TC-DVZZDATE-PntTab.
           05 TC-DVZZDATE-PntNbr         PIC S9(04) COMP VALUE 5.
      *DVZZDATE::currentDate
           05 TC-DVZZDATE-db42865c-Idt   PIC X(08) VALUE 'db42865c'.
           05 TC-DVZZDATE-db42865c PROCEDURE-POINTER.
      *DVZZDATE::currentDateDB2
           05 TC-DVZZDATE-fa5ee5e9-Idt   PIC X(08) VALUE 'fa5ee5e9'.
           05 TC-DVZZDATE-fa5ee5e9 PROCEDURE-POINTER.
      *DVZZDATE::currentDateJulian
           05 TC-DVZZDATE-cf63e86f-Idt   PIC X(08) VALUE 'cf63e86f'.
           05 TC-DVZZDATE-cf63e86f PROCEDURE-POINTER.
      *DVZZDATE::currentDateFreeFormat
           05 TC-DVZZDATE-cfc290ce-Idt   PIC X(08) VALUE 'cfc290ce'.
           05 TC-DVZZDATE-cfc290ce PROCEDURE-POINTER.
      *DVZZDATE::currentDateString
           05 TC-DVZZDATE-b8721d20-Idt   PIC X(08) VALUE 'b8721d20'.
           05 TC-DVZZDATE-b8721d20 PROCEDURE-POINTER.

       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.


      *=================================================================
       PROCEDURE DIVISION USING PntTab-Pnt.
                          
      *
      *    IF CallIsCopy
      *      PERFORM Copy-Process-Mode
      *    ELSE
           PERFORM FctList-Process-Mode
           perform INIT-LIBRARY
      *    END-IF

           GOBACK.

        FctList-Process-Mode.
            IF NOT TC-DVZZDATE-FctList-IsLoaded
              SET TC-DVZZDATE-db42865c   TO ENTRY 'db42865c'
              SET TC-DVZZDATE-fa5ee5e9   TO ENTRY 'fa5ee5e9'
              SET TC-DVZZDATE-cf63e86f   TO ENTRY 'cf63e86f'
              SET TC-DVZZDATE-cfc290ce   TO ENTRY 'cfc290ce'
              SET TC-DVZZDATE-b8721d20   TO ENTRY 'b8721d20'

              SET TC-DVZZDATE-FctList-IsLoaded TO TRUE
            END-IF
               .

            set PntTab-Pnt TO ADDRESS OF TC-DVZZDATE-PntTab

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
       PROGRAM-ID. db42865c.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
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
       END PROGRAM cf63e86f.
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
       PROGRAM-ID. cfc290ce.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
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
       01 Result PIC 9(08).
       PROCEDURE DIVISION
             USING BY REFERENCE Result
           .
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END PROGRAM b8721d20.
