       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZOSM7.
       DATA DIVISION .
       WORKING-STORAGE SECTION.

                      
       local-STORAGE SECTION.
      
       01 Var1.
           88 Value1              value '123456789'.
           05 JobName pic X(08).
           05 Site pic X.
              88 SiteTest value 'A'.
       LINKAGE SECTION.
       01 TC-FunctionCode pic X(30).
      * Function which call program a711ebb5
      * Which is generated code for DVZZOSM7.StartCheckpoint
           88 Fct-a711ebb5-StartCheckpoint
              value 'Fct=a711ebb5-StartCheckpoint'.

      
      
       PROCEDURE DIVISION USING TC-FunctionCode
                          .
           PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

       FctList-Process-Mode.
           evaluate true
              when Fct-a711ebb5-StartCheckpoint
                 call 'a711ebb5'
              when other
                 Perform Handle-Error
           end-evaluate
           .
       Handle-Error.
           continue
           .
                          
      
      *-----------------------------------------------------------------
      *declare procedure StartCheckpoint public.
       END PROGRAM DVZZOSM7.
      *
      *declare procedure StartCheckpoint public.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a711ebb5.
       data division.
       working-storage section.
      *DVZZOSM7.StartCheckpoint  - No Params
                               
      
       01 Var2.
           05 JobName pic X(08).
           05 Site pic X.
              88 SiteTest value 'A'.
      
      
       PROCEDURE DIVISION
           .
      *DVZZOSM7.StartCheckpoint  - No Params
      *    if Var2::JobName(1:2) = 'ET'
      *       or Var2::JobName(1:4) = 'PSAT'
           if JobName OF Var2(1:2) = 'ET'
              or JobName OF Var2(1:4) = 'PSAT'
              display "case 6"
           end-if
      
      *    if Var2::JobName(1:2) = 'ET'
      *       or Var2::Site::SiteTest
           if JobName OF Var2(1:2) = 'ET'
              or SiteTest OF Site OF Var2
              display "case 7"
           end-if
      
      *    if Var2::JobName(1:2) = 'ET'
      *       or Var2::JobName(1:4) = 'PSAT'
      *       or Var2::Site::SiteTest
           if JobName OF Var2(1:2) = 'ET'
              or JobName OF Var2(1:4) = 'PSAT'
              or SiteTest OF Site OF Var2
              display "case 8"
           end-if
      
           .
       END PROGRAM a711ebb5.


