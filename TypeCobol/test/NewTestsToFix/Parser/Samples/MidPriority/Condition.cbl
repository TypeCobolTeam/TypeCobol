*Don't except any errors 
 IDENTIFICATION DIVISION.                        
 PROGRAM-ID.     MyPGM.
 DATA DIVISION.                                  
 WORKING-STORAGE SECTION.                        
 01                PIC X.
        88 ASCII                 VALUE X'01'.    
        88 EBCDIC                VALUE X'02'.    
 PROCEDURE DIVISION.                             
* OK
     IF NOT (ASCII OR EBCDIC)                    
        DISPLAY 'Hello'
     END-IF                                      
     .                                           