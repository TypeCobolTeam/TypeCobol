Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
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

      *    if Var1::JobName(1:2) = 'ET'
           if JobName OF Var1(1:2) = 'ET'
               display "case 1"
           end-if

      *    if Var1::Site::SiteTest
           if SiteTest OF Site OF Var1
                          display "case 2"
           end-if

      *    if Var1::JobName(1:2) = 'ET'
      *      or Var1::JobName(1:4) = 'PSAT'
      *      or Var1::Site::SiteTest
           if JobName OF Var1(1:2) = 'ET'
             or JobName OF Var1(1:4) = 'PSAT'
             or SiteTest OF Site OF Var1
                          display "case 3"
           end-if
      *    if Var1::Value1 or Var1::Value1
           if Value1 OF Var1 or Value1 OF Var1
               display "case 4"
           end-if
           .
       END PROGRAM DVZZOSM7.
