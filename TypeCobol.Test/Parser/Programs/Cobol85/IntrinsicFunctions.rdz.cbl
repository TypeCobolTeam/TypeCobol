       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-INTRINSICS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
           IF FUNCTION ABS(1.0) > 0
             DISPLAY "ABS OK"
           END-IF

           IF FUNCTION BIT-OF('Hello, world!') > 0
             DISPLAY "BIT-OF OK"
           END-IF

           IF FUNCTION BIT-TO-CHAR('11001001') > 0
             DISPLAY "BIT-TO-CHAR OK"
           END-IF

           IF FUNCTION BYTE-LENGTH('Hello, world!') > 0
             DISPLAY "BYTE-LENGTH OK"
           END-IF

           IF FUNCTION COMBINED-DATETIME(143951, 18867.812479168304) > 0
             DISPLAY "COMBINED-DATETIME OK"
           END-IF

           IF FUNCTION E > 0
             DISPLAY "E OK"
           END-IF

           IF FUNCTION EXP(143951) > 0
             DISPLAY "EXP OK"
           END-IF

           IF FUNCTION EXP10(143951) > 0
             DISPLAY "EXP10 OK"
           END-IF

           IF FUNCTION FORMATTED-CURRENT-DATE('YYYYMMDDThhmmss.ss+hhmm')
             > 0
             DISPLAY "FORMATTED-CURRENT-DATE OK"
           END-IF

           IF FUNCTION FORMATTED-DATE('YYYYMMDD', 143951) > 0
             DISPLAY "BIT-TO-CHAR OK"
           END-IF

           IF FUNCTION FORMATTED-DATETIME('YYYYMMDDThhmmss.ss+hhmm',
             143951, 18867.812479168304) > 0
             DISPLAY "FORMATTED-DATETIME OK"
           END-IF

           IF FUNCTION FORMATTED-TIME('hhmmss.ss+hhmm',
             18867.812479168304) > 0
             DISPLAY "FORMATTED-TIME OK"
           END-IF

           IF FUNCTION HEX-OF('Hello, world!') > 0
             DISPLAY "HEX-OF OK"
           END-IF

           IF FUNCTION HEX-TO-CHAR('FFAABB') > 0
             DISPLAY "HEX-TO-CHAR OK"
           END-IF

           IF FUNCTION INTEGER-OF-FORMATTED-DATE('YYYYMMDD', '19950215')
             > 0
             DISPLAY "INTEGER-OF-FORMATTED-DATE OK"
           END-IF

           IF FUNCTION NUMVAL-F(' +789 ') > 0
             DISPLAY "NUMVAL-F OK"
           END-IF

           IF FUNCTION PI > 0
             DISPLAY "PI OK"
           END-IF

           IF FUNCTION SECONDS-FROM-FORMATTED-TIME('hhmmss.ss+hhmm',
             '05142781+0500') > 0
             DISPLAY "SECONDS-FROM-FORMATTED-TIME OK"
           END-IF

           IF FUNCTION SECONDS-PAST-MIDNIGHT > 0
             DISPLAY "SECONDS-PAST-MIDNIGHT OK"
           END-IF

           IF FUNCTION SIGN(11) > 0
             DISPLAY "SIGN OK"
           END-IF

           IF FUNCTION TEST-DATE-YYYYMMDD(19950215) = 0
             DISPLAY "TEST-DATE-YYYYMMDD OK"
           END-IF

           IF FUNCTION TEST-DAY-YYYYDDD(1995146) = 0
             DISPLAY "TEST-DAY-YYYYDDD OK"
           END-IF

           IF FUNCTION TEST-FORMATTED-DATETIME('YYYYMMDD', '19950215')
             = 0
             DISPLAY "TEST-FORMATTED-DATETIME OK"
           END-IF

           IF FUNCTION TEST-NUMVAL('0 1') = 3
             DISPLAY "TEST-NUMVAL OK"
           END-IF

           IF FUNCTION TEST-NUMVAL-C('+ EUR 1000', 'EUR') = 0
             DISPLAY "TEST-NUMVAL-C OK"
           END-IF

           IF FUNCTION TEST-NUMVAL-F(' + 100') = 0
             DISPLAY "TEST-NUMVAL-F OK"
           END-IF

           IF FUNCTION TRIM(" Hello ") = "Hello"
             DISPLAY "TRIM OK"
           END-IF

           IF FUNCTION TRIM(" Hello ", LEADING) = "Hello "
             DISPLAY "TRIM LEADING OK"
           END-IF

           IF FUNCTION TRIM(" Hello ", TRAILING) = " Hello"
             DISPLAY "TRIM TRAILING OK"
           END-IF
           
           IF FUNCTION UUID4 > 0
             DISPLAY "UUID4 OK"
           END-IF

            GOBACK.
       END PROGRAM PGM-INTRINSICS.