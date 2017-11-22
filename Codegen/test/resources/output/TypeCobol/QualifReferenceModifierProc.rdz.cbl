       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZOSM7.
       DATA DIVISION .
       WORKING-STORAGE SECTION.
       01  TC-DVZZOSM7-FctList-Loaded PIC X(02).
           88 TC-DVZZOSM7-FctList-IsLoaded      VALUE 'OK'.
       01 TC-DVZZOSM7-PntTab.
           05 TC-DVZZOSM7-PntNbr         PIC S9(04) COMP VALUE 1.
      *DVZZOSM7::StartCheckpoint
           05 TC-DVZZOSM7-a711ebb5-Idt   PIC X(08) VALUE 'a711ebb5'.
           05 TC-DVZZOSM7-a711ebb5 PROCEDURE-POINTER.

                      
       local-STORAGE SECTION.
      
       01 Var1.
           88 Value1              value '123456789'.
           05 JobName pic X(08).
           05 Site pic X.
              88 SiteTest value 'A'.
       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.

      
      
       PROCEDURE DIVISION USING PntTab-Pnt.
                          
      *
      *    IF CallIsCopy
      *      PERFORM Copy-Process-Mode
      *    ELSE
           PERFORM FctList-Process-Mode
           perform INIT-LIBRARY
      *    END-IF

           GOBACK.

        FctList-Process-Mode.
            IF NOT TC-DVZZOSM7-FctList-IsLoaded
              SET TC-DVZZOSM7-a711ebb5   TO ENTRY 'a711ebb5'

              SET TC-DVZZOSM7-FctList-IsLoaded TO TRUE
            END-IF
               .

            set PntTab-Pnt TO ADDRESS OF TC-DVZZOSM7-PntTab

           .
                          
      
      *-----------------------------------------------------------------
      *declare procedure StartCheckpoint public.
       END PROGRAM DVZZOSM7.
      *
      *declare procedure StartCheckpoint public.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a711ebb5StartCheckpoint.
       data division.
       working-storage section.
      
       01 Var2.
           05 JobName pic X(08).
           05 Site pic X.
              88 SiteTest value 'A'.
      
      
       PROCEDURE DIVISION
           .
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
       END PROGRAM a711ebb5StartCheckpoint.
