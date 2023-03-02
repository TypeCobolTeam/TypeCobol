 IDENTIFICATION DIVISION.                                     
 PROGRAM-ID. ReplaceWithSpace2.                                        
 DATA DIVISION.                                               
 WORKING-STORAGE SECTION.                                     

 REPLACE  == :MAXTAB :==  BY ==1== .

 01 DATES-ARRETS.
   05 FILLER PIC 9(6) VALUE  200905 .
 01 TAB-DATES REDEFINES DATES-ARRETS.
   05 DATES                   PIC 9(6) OCCURS :MAXTAB:.

 REPLACE OFF.

 PROCEDURE DIVISION.                                          
     .
 END PROGRAM  ReplaceWithSpace2.
