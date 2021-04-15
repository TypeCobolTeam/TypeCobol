000000 IDENTIFICATION DIVISION.
000000 PROGRAM-ID.   FileControl.
000000 ENVIRONMENT DIVISION.                      
000000 CONFIGURATION SECTION.                      
000000 SOURCE-COMPUTER. IBM-370                    
000000      .                                      
000000 OBJECT-COMPUTER. IBM-370.                   
000000 SPECIAL-NAMES. DECIMAL-POINT IS COMMA.      
000000 INPUT-OUTPUT SECTION.                                      
000000 FILE-CONTROL.                                              
000000     SELECT  FAPPEL  ASSIGN TO UT-S-FAPPEL.                 
000000                                                            
000000 DATA DIVISION.                                             
000000 FILE SECTION.                                              
000000 FD  FAPPEL BLOCK 0 RECORDS                                 
000000            LABEL RECORD STANDARD                           
000000            RECORDING MODE F.                               
000000 01  FIC-APPEL PIC X(128).                                  
000000 
000000 WORKING-STORAGE SECTION.
000000 01 MyData pic X.
000000     88 MyData-val1 value '1'.
000000     88 MyData-val2 value '2'.
000000     88 MyData-val3 value '3'.
000000 
000000 PROCEDURE DIVISION.
000000******************
000000    evaluate true
000000     when MyData = "A" 
000000       move "1" to MyData
000000     when MyData = "A"
000000       move "2" to MyData
000000     when MyData = "A"
000000       move "3" to MyData
000000     when other
000000       move "4" to MyData
000000    end-evaluate
000000  .
