Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
       CBL ARITH(EXTEND)
       CBL NOLIST
      *CONTROL LIST
       CBL FLAG(I,W)
      *TypeCobol_Version:TestTypeCobolVersion
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZXERRM.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 Counter PIC 9999.
      *CONTROL NOLIST
       PROCEDURE DIVISION.

           MOVE 10 TO Counter.
      *CBL NOSOURCE
       END PROGRAM DVZXERRM.
