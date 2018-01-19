000000*Don't except any errors 
001540 IDENTIFICATION DIVISION.                      
001570 PROGRAM-ID.     MyPGM.
002090 DATA DIVISION.                                
002860 WORKING-STORAGE SECTION.                      
005510 01                PIC X.
005520        88 ASCII-v                 VALUE X'01'.  
005530        88 EBCDIC-v                VALUE X'02'.  
032810 PROCEDURE DIVISION.                           
      * OK
143400     IF NOT (ASCII-v OR EBCDIC-v)                  
143410        DISPLAY 'Hello'
143470     END-IF                                    
143480     .                                         