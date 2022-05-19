       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcCall.
                                                                        
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM_NAME      pic X(08) value 'PGM00001'.
       01 PARAMETER_1       pic X(08) value 'NotPgm1'.
       01 PARAMETER_2       pic X(08) value 'NotPgm2'.
       01 PGM_NAME_TMP      pic X(08).
                                                                        
       PROCEDURE DIVISION.
                                                                        
            call 'zcallpgm' using PROGRAM_NAME
                                PARAMETER_1
                                PARAMETER_2.
                                                                        
            call 'ZCALLPGM' using by content PROGRAM_NAME
                                PARAMETER_1.
                                                                        
            PERFORM CHANGEPGM1.
            call 'zCaLlpgm' using by reference PROGRAM_NAME.
                                                                        
            call 'zCaLlpgm' using by value PROGRAM_NAME.
                                                                        
            call 'test' using by value PROGRAM_NAME.
                                                                        
            PERFORM CHANGETMP3.
            call 'zCaLlpgF' using PROGRAM_NAME.
                                                                        
            call 'ZCALLPGG' using by content PROGRAM_NAME
                                PARAMETER_1.
                                                                        
            call 'zcallpgr' using PGM_NAME_TMP
                                PARAMETER_1
                                PARAMETER_2.
                                                                        
           MOVE 'PGML00003' TO PROGRAM_NAME
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
           MOVE 'PGM00002' TO PROGRAM_NAME
           .
                                                                        
       CHANGETMP.
           MOVE 'PGMTMP' TO PGM_NAME_TMP
           MOVE PGM_NAME_TMP TO PROGRAM_NAME
           .
                                                                        
       CHANGETMP3.
           MOVE 'PGMTMP3' TO PGM_NAME_TMP
           .
       END PROGRAM ProcCall.
                                                                        
                                                                        
      