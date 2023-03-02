IDENTIFICATION DIVISION.                                      
PROGRAM-ID.  ReplaceWithSpace3.                                        
DATA DIVISION.                                                
WORKING-STORAGE SECTION.                                      


REPLACE ==:NB-EXTRACT-MAX:== BY ==150==.                      
01  IND-EXTRACT            PIC  9(4) COMP.                    
    88  IND-EXTRACT-OK     VALUE 1 THRU : NB-EXTRACT-MAX:.    
REPLACE OFF.                                                  
PROCEDURE DIVISION.
    .                                                         
END PROGRAM ReplaceWithSpace3.                                         