       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcCall.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM_NAME      pic X(08) value 'PGM00000'.
       01 I pic 9(03) value 0.
      
       PROCEDURE DIVISION.
           perform varying I from 1 by 1 until I < 10
               if(I > 50) 
                    move "MyPgm" to PROGRAM_NAME
                end-if  
                call 'zcallpgm' using PROGRAM_NAME
                move "MyPgm2" to PROGRAM_NAME
                if(I > 50) 
                    perform Set-MyPgm3
                end-if
           end-perform
           call 'zcallpgm' using PROGRAM_NAME
           .
       Set-MyPgm3.
           move "MyPgm3" to PROGRAM_NAME
           .
      
       END PROGRAM ProcCall.  