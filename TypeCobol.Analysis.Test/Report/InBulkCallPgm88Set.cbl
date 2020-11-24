       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcCall.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM_NAME      pic X(08) value 'PGM00000'.
          88 pgm1 VALUE 'PGM00001'.
          88 pgm2 VALUE 'PGM00002'.
          88 pgm3 VALUE 'PGM00003'.
       01 PARAMETER_1       pic X(08) value 'NotPgm1'.
       01 PARAMETER_2       pic X(08) value 'NotPgm2'.
       01 PGM_NAME_TMP      pic X(08).
      
       PROCEDURE DIVISION.
      
           call 'zcallpgm' using PROGRAM_NAME
                                PARAMETER_1
                                PARAMETER_2.
      
           PERFORM CHANGEPGM1.
           call 'zCaLlpgm' using by reference PROGRAM_NAME.
      
           PERFORM CHANGEPGM2.
           call 'zCaLlpgF' using PROGRAM_NAME.
      
      
           PERFORM CHANGEPGM3.
           call 'zcallpgr' using PROGRAM_NAME
                        PARAMETER_1
                        PARAMETER_2.
      
           MOVE 'PGM00004' TO PROGRAM_NAME
           call 'zCaLlpgt' using by reference PROGRAM_NAME.
      
           PERFORM CHANGETMP.
           call 'zCaLlpgx' using by reference PROGRAM_NAME.
      
           call 'zCaLlsrv' using by reference PGM_NAME_TMP.
      
           evaluate true
           when "1" = "1"
                continue
           when other
                continue
           end-evaluate.
           EXIT.
      
       CHANGEPGM1.
           SET PGM1 to true.
      
       CHANGEPGM2.
           SET PGM2 to true.
      
       CHANGEPGM3.
           SET PGM3 to true.
      
       CHANGETMP.
           MOVE 'PGMTMP' TO PGM_NAME_TMP
           MOVE PGM_NAME_TMP TO PROGRAM_NAME
           .
      
       END PROGRAM ProcCall.
      