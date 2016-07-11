*Don't except any error here
 ID DIVISION.                                                     
 PROGRAM-ID. MyPGM.                                               
 ENVIRONMENT DIVISION.                                            
 DATA DIVISION.                                                   
 WORKING-STORAGE SECTION.                                         
 01 var1                              pic X(30).
 01 FILLER.                                                       
     02  ZONE1.                                                   
         04  AAAA                     PIC 9(04).                  
         04  MM                       PIC 9(02).                  
         04  JJ                       PIC 9(02).                  
                                                                  
 PROCEDURE DIVISION.
           MOVE MOIS(MM IN ZONE1)     TO var1
           MOVE AAAA IN ZONE1         TO var1
     .