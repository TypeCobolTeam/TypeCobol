﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. CASE_IF.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
       01  T1-A.
           05  T1-B1   PIC X.
           05  T1-B2   PIC X.
      
       PROCEDURE DIVISION.
           IF ((T1-A :: T1-B1) > 1)
             DISPLAY "IF1"
          ELSE
             CONTINUE
          END-IF.
      
           IF ((T1-A :: T1-B1) > 1 AND (T1-A :: T1-B2) > 15)
             DISPLAY "IF2"
          ELSE
             CONTINUE
          END-IF.
      
           IF ((T1-A :: T1-B1) > 1 AND
                (T1-A :: T1-B2) > 32)
             DISPLAY "IF3"
          ELSE
             CONTINUE
          END-IF.
      
       END PROGRAM CASE_IF.
      