      ****** Support REPOSITORY PARAGRAPH *****
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pgm2536.
       FUNCTION ABS INTRINSIC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       REPOSITORY.
           Class Base is "java.lang.Object"
           Class Customer is "com.ei.Customer"
           FUNCTION ALL ABS INTRINSIC
           FUNCTION INVALID-NAME INTRINSIC.
       DATA DIVISION.
           FUNCTION ABS INTRINSIC.
       WORKING-STORAGE  SECTION.
       PROCEDURE DIVISION.
           FUNCTION ABS TRIM INTRINSIC.
            
            IF FUNCTION ABS(10) PI > 0
              DISPLAY "PI OK"
            END-IF
            GOBACK.
       END PROGRAM pgm2536.