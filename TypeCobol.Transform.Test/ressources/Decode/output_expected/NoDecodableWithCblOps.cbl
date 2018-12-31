       CBL ARITH(EXTEND)
      *CONTROL NOLIST
      *CBL NOSOURCE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZXERRM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9999.
       01 MyData TYPE DateDB2.
       PROCEDURE DIVISION.
      *     MOVE 10 TO COUNTER.
      *     CALL TCOZDATE::CurrentDate OUTPUT MyData.
      *     CALL MyProc.
           DECLARE PROCEDURE MyProc public.
                DATA DIVISION.
                WORKING-STORAGE SECTION.
                01 MyData TYPE DateDB2.
                PROCEDURE DIVISION.
                    CALL TCOZDATE::CurrentDate OUTPUT MyData.
                    DISPLAY 'TOTO IS HERE'.
      
           END-DECLARE.
      * CBL NOSOURCE.
      
       END PROGRAM DVZXERRM.