       IDENTIFICATION DIVISION.
       PROGRAM-ID. tcCall.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var-program-name PIC X(8) VALUE 'somepgm'.
       01 var-input PIC X.
       01 var-inout PIC X.
       01 var-output PIC X.
       PROCEDURE DIVISION.
      *OK
           CALL var-program-name USING var-input var-inout var-output.
      *KO procedure-style CALL
           CALL var-program-name INPUT var-input 
                                 IN-OUT var-inout
                                 OUTPUT var-output.
       END PROGRAM tcCall.