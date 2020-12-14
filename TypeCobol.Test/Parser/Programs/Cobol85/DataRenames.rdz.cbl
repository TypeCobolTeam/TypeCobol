       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TEST-GROUP1.
          05 TEST-SGROUP1.
             10 TEST-ELT1 PIC X(04).
             10 TEST-ELT2 PIC X(350).
          05 TEST-SGROUP2.
             10 TEST-ELT3 PIC X(200).
             10 TEST-ELT4 PIC X(1000).
          05 TEST-SGROUP5.
             10 TEST-ELT5 PIC X(100).
          66 TEST-RENAMES RENAMES TEST-ELT2
                          THRU    TEST-ELT4.
       END PROGRAM PGM1.