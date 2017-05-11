       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZOSM7.
       DATA DIVISION .
       local-STORAGE SECTION.

       01 Var1.
           88 Value1              value '123456789'.
           05 JobName pic X(08).
           05 Site pic X.
              88 SiteTest value 'A'.


       PROCEDURE DIVISION.

           if Var1::JobName(1:2) = 'ET'
               display "case 1"
           end-if

           if Var1::Site::SiteTest
                          display "case 2"
           end-if

           if Var1::JobName(1:2) = 'ET'
             or Var1::JobName(1:4) = 'PSAT'
             or Var1::Site::SiteTest
                          display "case 3"
           end-if
           if Var1::Value1 or Var1::Value1
               display "case 4"
           end-if
           .
       END PROGRAM DVZZOSM7.
