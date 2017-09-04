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

       01  dateJulian    TYPEDEF strict.
           10 YYYY                   PIC 9(04).
           10 DDD                    PIC 9(03).

       01  dateDB2       TYPEDEF strict.
           10 YYYY                   PIC 9(04).
           10                        PIC X(01).
           10 MM                     PIC 9(02).
           10                        PIC X(01).
           10 DD                     PIC 9(02).

       01  dateString    TYPEDEF strict  PIC 9(08).

       01 culture        TYPEDEF strict.
           10 lng                    PIC X(02).
           10 cty                    PIC X(02).

      *=================================================================
       PROCEDURE DIVISION.
      *=================================================================
       DECLARE FUNCTION currentDate PUBLIC
      *Description of currentDate
           RETURNING Result TYPE date.
       PROCEDURE DIVISION.
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END-DECLARE.
      *_________________________________________________________________
       DECLARE FUNCTION currentDateDB2 PUBLIC
           RETURNING Result Type dateDB2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-Dat       TYPE date.

       PROCEDURE DIVISION.

           ACCEPT W-Dat             FROM DATE YYYYMMDD
           move W-Dat :: YYYY       to Result :: YYYY
           move W-Dat :: MM         to Result :: MM
           move W-Dat :: DD         to Result :: DD 
      *    move '-'                 to Result(5:1)
      *    move '-'                 to Result(8:1)
           
           .
       END-DECLARE.
      *_________________________________________________________________
       DECLARE FUNCTION currentDateJulian PUBLIC
      * my comment
           RETURNING Result Type dateJulian.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-Dat       TYPE date.

       PROCEDURE DIVISION.

           ACCEPT W-Dat             FROM DATE YYYYMMDD
           move unsafe W-Dat to Result
           .
       END-DECLARE.
      *_________________________________________________________________
       DECLARE FUNCTION currentDateFreeFormat PUBLIC
                          INPUT dateType   PIC X(01)
                                direction  PIC X(01)
                                separator  PIC X(01)

                                culture    TYPE culture
                                returnCode PIC 9(04)
                          RETURNING Result PIC X(40).
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-Dat       TYPE date.

       PROCEDURE DIVISION.

           continue
           .
       END-DECLARE.
      *_________________________________________________________________
      *Keep spaces at end of line, because there were 
      * presents in source file                      
       DECLARE FUNCTION currentDateString PUBLIC     
           RETURNING Result TYPE dateString.         
       PROCEDURE DIVISION.
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END-DECLARE.

       END PROGRAM DVZZDAT.
