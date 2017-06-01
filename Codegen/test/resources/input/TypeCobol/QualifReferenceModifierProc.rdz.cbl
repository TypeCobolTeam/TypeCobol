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
      
      *-----------------------------------------------------------------
       declare procedure StartCheckpoint public.
       data division.
       working-storage section.
      
       01 Var2.
           05 JobName pic X(08).
           05 Site pic X.
              88 SiteTest value 'A'.
      
       procedure division.
      
           if Var2::JobName(1:2) = 'ET'
              or Var2::JobName(1:4) = 'PSAT'
              display "case 6"
           end-if
      
           if Var2::JobName(1:2) = 'ET'
              or Var2::Site::SiteTest
              display "case 7"
           end-if
      
           if Var2::JobName(1:2) = 'ET'
              or Var2::JobName(1:4) = 'PSAT'
              or Var2::Site::SiteTest
              display "case 8"
           end-if
      
           .
       end-declare.
       END PROGRAM DVZZOSM7.