000000*Don't except any error here
000380 ID DIVISION.                                                     
000390 PROGRAM-ID. MyPGM.                                               
000780 ENVIRONMENT DIVISION.                                            
000890 DATA DIVISION.                                                   
000940 WORKING-STORAGE SECTION.                                         
004370 01 var1                              pic X(30).
       01 moirsArray.
           05 mois pic X(30) occurs 12.
004380 01 FILLER.                                                       
004410     02  ZONE1.                                                   
004420         04  AAAA                     PIC 9(04).                  
004430         04  MM                       PIC 9(02).                  
004440         04  JJ                       PIC 9(02).                  
004510                                                                  
016720 PROCEDURE DIVISION.
041850           MOVE MOIS(MM IN ZONE1)     TO var1
                 if MOIS(MM IN ZONE1) = spaces
                   continue
                 end-if
041860           MOVE AAAA IN ZONE1         TO var1
043330     .