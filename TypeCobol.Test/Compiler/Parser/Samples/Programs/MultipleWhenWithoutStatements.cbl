﻿ IDENTIFICATION DIVISION.
 PROGRAM-ID. MYPGM.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-370.
 PROCEDURE DIVISION.
           evaluate true
             when TST-STT-KO
               add 1                            to NBR-ERR
             when TST-STT-OK
               add 1                            to NBR-OK
*KO: when statement before a when other can't be empty			   
             when TST-STT-UNKNOWN
             when other
                perform ABEND
           end-evaluate
     .
 END PROGRAM MYPGM.