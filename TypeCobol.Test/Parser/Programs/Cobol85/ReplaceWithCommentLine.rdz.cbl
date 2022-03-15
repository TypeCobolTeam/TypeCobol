       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
            Replace
             ==:Test:==                      By
             ==
               When Other
      ********** Move SPACES
                 Continue
             ==
            .
       WORKING-STORAGE SECTION.
       01 var1 PIC X.
          88 var1-A VALUE 'A'.
       PROCEDURE DIVISION.
           EVALUATE TRUE
           WHEN var1-A
             DISPLAY 'A'
           :test:
           END-EVALUATE
           GOBACK
           .
       END PROGRAM MyPGM.