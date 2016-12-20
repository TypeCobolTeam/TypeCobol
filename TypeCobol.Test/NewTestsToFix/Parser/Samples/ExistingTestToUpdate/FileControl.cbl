 *Replace the existing FileControl test in Samples/Programs/Cobol85
 IDENTIFICATION DIVISION.
 PROGRAM-ID.   FileControl.
 ENVIRONMENT DIVISION.                      
 CONFIGURATION SECTION.                      
 SOURCE-COMPUTER. IBM-370                    
      .                                      
 OBJECT-COMPUTER. IBM-370.                   
 SPECIAL-NAMES. DECIMAL-POINT IS COMMA.      
 INPUT-OUTPUT SECTION.                                      
 FILE-CONTROL.                                              
     SELECT  FAPPEL  ASSIGN TO UT-S-FAPPEL.                 
                                                            
 DATA DIVISION.                                             
 FILE SECTION.                                              
 FD  FAPPEL BLOCK 0 RECORDS                                 
            LABEL RECORD STANDARD                           
            RECORDING MODE F.                               
 01  FIC-APPEL PIC X(128).
 
*"Record are" is permitted by IBM Compiler V4
 FD  FENT010  LABEL RECORD ARE STANDARD     
              BLOCK  CONTAINS   0 RECORDS   
              RECORD CONTAINS 699 CHARACTERS
              RECORDING MODE IS F           
              DATA RECORD IS ENTREE.                                     
 01  FIC-APPEL2 PIC X(128).                                  

 WORKING-STORAGE SECTION.
 01 MyData pic X.
     88 MyData-val1 value '1'.
     88 MyData-val2 value '2'.
     88 MyData-val3 value '3'.

 PROCEDURE DIVISION.
******************
    evaluate true
     when MyData = "A" 
       move "1" to MyData
     when MyData = "A"
       move "2" to MyData
     when MyData = "A"
       move "3" to MyData
     when other
       move "4" to MyData
    end-evaluate
  .