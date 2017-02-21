       IDENTIFICATION DIVISION.
       PROGRAM-ID. B110PGM1.
      *REMARKS. COPY=(
      * ).ddddddd
       ENVIRONMENT DIVISION.sss
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370ffff
      *               WITH DEBccUGGING MODE
                      .
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.xxx

       WORKING-STORAGE SECTION.
       77  NOM-PGM                           PIC X(08) VALUE 'B110PGM1'.

       01 W-XXXX     fff                    d PIC X(10).

       01 InternalReference  TYPEDEF strong.
           05 InternalReferenceType       pic X(03).
           05 InternalReferenceValue      pic X(13).s
      *
       01  bankAccountIntRef TYPE InternalReference.

       01  date1 PIC 9(08).
      *01  date1 TYPE Date.
      *01  date2 TYPE Date.
      *01  dateJu TYPE DateJulian.
       01  nbOfDays pic 9(08).
       01  maDateFormatInconnu pic 9(08).
      *01  currentAccountBalance TYPE Amount.

       LINKAGE SECTION.

       01  DVZE00                        pic X.
       01  DVZS00                        pic X.

       PROCEDURE DIVISION.

           MOVE W-eeXXXX              TO bankAccountIntRef
           move maDateFormatInconnu to date1


      *    move function currentDate()  to  date2
      *    move date2 to date1
      *    move function currentDateJulian() to dateJu
      *
      *    move function daysBetween2(date1, date2) to nbOfDays


           goback
           .
       END PROGRAM B110PGM1.