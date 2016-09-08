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

       01  dateJulian    TYPEDEF.
           10 YYYY                   PIC 9(04).
           10 DDD                    PIC 9(03).

       01  dateDB2       TYPEDEF.
           10 YYYY                   PIC 9(04).
           10                        PIC X(01) VALUE '-'.
           10 MM                     PIC 9(02).
           10                        PIC X(01) VALUE '-'.
           10 DD                     PIC 9(02).

       01 culture        TYPEDEF.
           10 lng                    PIC X(02).
           10 cty                    PIC X(02).
      *_________________________________________________________________
       LINKAGE SECTION.
           COPY YDVZDAT REPLACING ==:DVZDAT:== BY ==DVZDAT==.

      *=================================================================
       PROCEDURE DIVISION USING DVZDAT.
      *=================================================================
       DECLARE FUNCTION currentDate PUBLIC.

       PROCEDURE DIVISION RETURNING Result TYPE date.
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END-DECLARE.
      *_________________________________________________________________
       DECLARE FUNCTION currentDateDB2 PUBLIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-Dat       TYPE date.

       PROCEDURE DIVISION RETURNING Result Type dateDB2.

           ACCEPT W-Dat             FROM DATE YYYYMMDD
           MOVE CORR W-Dat          TO Result
           .
       END-DECLARE.
      *_________________________________________________________________
       DECLARE FUNCTION currentDateJulian PUBLIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-Dat       TYPE date.

       PROCEDURE DIVISION RETURNING Result Type dateJulian.

           ACCEPT W-Dat             FROM DATE YYYYMMDD
           MOVE FUNCTION DAY-OF-INTEGER
                         (FUNCTION INTEGER-OF-DATE(W-Dat))
                TO Result
           .
       END-DECLARE.
      *_________________________________________________________________
       DECLARE FUNCTION currentDateFreeFormat PUBLIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-ZDAT2000               PIC X(08) VALUE 'ZDAT2000'.
       01  DATS20. COPY YDATS20.
       01  W-Dat       TYPE date.

       PROCEDURE DIVISION INPUT dateType   PIC X(01)
                                direction  PIC X(01)
                                separator  PIC X(01)
                                culture    TYPE culture
                                returnCode PIC 9(04)
                          RETURNING Result PIC X(40).

           MOVE SPACES                       TO DATS20

           SET DATS20-I-FONCTION-FORMATAGE   TO TRUE
           MOVE 'JOUR'                       TO DATS20-I-DATE1
           MOVE dateType                     TO DATS20-I-RETOUR-TYPE1
           MOVE direction                    TO DATS20-I-RETOUR-SENS1
           MOVE separator                    TO DATS20-I-RETOUR-SEPAR
           MOVE culture :: lng               TO DATS20-I-INT-LANG
           MOVE culture :: cty               TO DATS20-I-INT-PAYS
           MOVE 'M'                          TO DATS20-I-POLICE
           MOVE 'P'                          TO DATS20-I-INJOUR
           SET DATS20-I-DATE1-SSAAMMJJ-OUI   TO TRUE

           CALL 'ZCALLPGM' USING C-ZDAT2000
                                 DATS20

           IF DATS20-O-ERREUR
               MOVE ALL '9'                  TO returnCode
               DISPLAY DATS20-O-LIBRET
           ELSE
               MOVE DATS20-O-DATE-LONG       TO Result
           END-IF
           .
       END-DECLARE.

       END PROGRAM DVZZDAT.
