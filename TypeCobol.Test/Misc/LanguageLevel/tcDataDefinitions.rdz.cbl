       IDENTIFICATION DIVISION.
       PROGRAM-ID. tcDataDef.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      %<<<
       @TODO Check that we get the correct error messages !
      %>>>
       01 MyType TYPEDEF STRICT.
          05 item PIC X.
       01 MyBool TYPE BOOL VALUE TRUE.
      %<<<
       @TODO Not allowed !
      %>>>
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM tcDataDef.