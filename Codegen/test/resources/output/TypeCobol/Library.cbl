      * 15 CodeElements errors
      * "1"@(39:8>39:16): [27:1] Syntax error : Illegal default section in library.
      * "1"@(41:12>41:67): [27:1] Syntax error : Symbol DVZDAT-currentDate is not referenced
      * "1"@(42:12>42:67): [27:1] Syntax error : Symbol DVZDAT-currentDateDB2 is not referenced
      * "1"@(43:12>43:67): [27:1] Syntax error : Symbol DVZDAT-currentDateJulian is not referenced
      * "1"@(44:12>44:67): [27:1] Syntax error : Symbol DVZDAT-currentDateFreeFormat is not referenced
      * "1"@(76:12>78:25): [27:1] Syntax error : Symbol DAY-OF-INTEGER is not referenced
      * "1"@(99:12>99:52): [27:1] Syntax error : Symbol DATS20-I-FONCTION-FORMATAGE is not referenced
      * "1"@(100:12>100:62): [27:1] Syntax error : Symbol DATS20-I-DATE1 is not referenced
      * "1"@(101:12>101:69): [27:1] Syntax error : Symbol DATS20-I-RETOUR-TYPE1 is not referenced
      * "1"@(102:12>102:69): [27:1] Syntax error : Symbol DATS20-I-RETOUR-SENS1 is not referenced
      * "1"@(103:12>103:69): [27:1] Syntax error : Symbol DATS20-I-RETOUR-SEPAR is not referenced
      * "1"@(106:12>106:63): [27:1] Syntax error : Symbol DATS20-I-POLICE is not referenced
      * "1"@(107:12>107:63): [27:1] Syntax error : Symbol DATS20-I-INJOUR is not referenced
      * "1"@(108:12>108:52): [27:1] Syntax error : Symbol DATS20-I-DATE1-SSAAMMJJ-OUI is not referenced
      * "1"@(117:16>117:54): [27:1] Syntax error : Symbol DATS20-O-DATE-LONG is not referenced
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZDATE.
       AUTHOR. REYDELPA.

      *=================================================================
       ENVIRONMENT DIVISION.
      *=================================================================
       CONFIGURATION SECTION.
      *_________________________________________________________________
      *SOURCE-COMPUTER.    IBM-3033 WITH DEBUGGING MODE.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
      *=================================================================
       DATA DIVISION.
      *=================================================================
       WORKING-STORAGE SECTION.
       77  C-WSS                     PIC X(03) VALUE 'WSS'.
       01  W-IfrPgm.
           05 C-PgmNme               PIC X(08) Value 'LIBDATE'.

      *01  dateJulian    TYPEDEF.                                             
      *    10 YYYY                   PIC 9(04).                               
      *    10 DDD                    PIC 9(03).                               

      *01  dateDB2       TYPEDEF.                                             
      *    10 YYYY                   PIC 9(04).                               
      *    10                        PIC X(01) VALUE '-'.                     
      *    10 MM                     PIC 9(02).                               
      *    10                        PIC X(01) VALUE '-'.                     
      *    10 DD                     PIC 9(02).                               

      *01 culture        TYPEDEF.                                             
      *    10 lng                    PIC X(02).                               
      *    10 cty                    PIC X(02).                               
      *_________________________________________________________________
       LINKAGE SECTION.

      *=================================================================
       PROCEDURE DIVISION USING DVZDAT.
      *=================================================================
           SET DVZDAT-currentDate               TO ENTRY 'CURRE001'
           SET DVZDAT-currentDateDB2            TO ENTRY 'CURRE002'
           SET DVZDAT-currentDateJulian         TO ENTRY 'CURRE003'
           SET DVZDAT-currentDateFreeFormat     TO ENTRY 'CURRE004'
           .
      *_________________________________________________________________
      *DECLARE FUNCTION currentDate PUBLIC.                                   
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. currentDate.                                               
       LINKAGE SECTION.                                                       
       01 Result.                                                             
           02 YYYY PIC 9(4).                                                  
           02 MM PIC 9(2).                                                    
           02 DD PIC 9(2).                                                    
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE Result                                        
         .                                                                    

           ACCEPT Result FROM DATE YYYYMMDD
           .
       END PROGRAM currentDate.                                               
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateDB2 PUBLIC.                                
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. currentDateDB2.                                            

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01  W-Dat       TYPE date.                                             
       01 W-Dat.                                                              
           02 YYYY PIC 9(4).                                                  
           02 MM PIC 9(2).                                                    
           02 DD PIC 9(2).                                                    
       LINKAGE SECTION.                                                       
       01 Result.                                                             
           02 YYYY PIC 9(04).                                                 
           02 PIC X(01).                                                      
           02 MM PIC 9(02).                                                   
           02 PIC X(01).                                                      
           02 DD PIC 9(02).                                                   
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE Result                                        
         .                                                                    


           ACCEPT W-Dat             FROM DATE YYYYMMDD
           MOVE CORR W-Dat          TO Result
           .
       END PROGRAM currentDateDB2.                                            
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateJulian PUBLIC.                             
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. currentDateJulian.                                         

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01  W-Dat       TYPE date.                                             
       01 W-Dat.                                                              
           02 YYYY PIC 9(4).                                                  
           02 MM PIC 9(2).                                                    
           02 DD PIC 9(2).                                                    
       LINKAGE SECTION.                                                       
       01 Result.                                                             
           02 YYYY PIC 9(04).                                                 
           02 DDD PIC 9(03).                                                  
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE Result                                        
         .                                                                    


           ACCEPT W-Dat             FROM DATE YYYYMMDD
           MOVE FUNCTION DAY-OF-INTEGER
                         (FUNCTION INTEGER-OF-DATE(W-Dat))
                TO Result
           .
       END PROGRAM currentDateJulian.                                         
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateFreeFormat PUBLIC.                         
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. currentDateFreeFormat.                                     

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-ZDAT2000               PIC X(08) VALUE 'ZDAT2000'.
       01  DATS20. COPY YDATS20.
      *01  W-Dat       TYPE date.                                             
       01 W-Dat.                                                              
           02 YYYY PIC 9(4).                                                  
           02 MM PIC 9(2).                                                    
           02 DD PIC 9(2).                                                    
       LINKAGE SECTION.                                                       
       01 dateType PIC X(01).                                                 
       01 direction PIC X(01).                                                
       01 separator PIC X(01).                                                
       01 culture.                                                            
           02 lng PIC X(02).                                                  
           02 cty PIC X(02).                                                  
       01 returnCode PIC 9(04).                                               
       01 Result PIC X(40).                                                   
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE dateType                                      
                   BY REFERENCE direction                                     
                   BY REFERENCE separator                                     
                   BY REFERENCE culture                                       
                   BY REFERENCE returnCode                                    
                   BY REFERENCE Result                                        
         .                                                                    


           MOVE SPACES                       TO DATS20

           SET DATS20-I-FONCTION-FORMATAGE   TO TRUE
           MOVE 'JOUR'                       TO DATS20-I-DATE1
           MOVE dateType                     TO DATS20-I-RETOUR-TYPE1
           MOVE direction                    TO DATS20-I-RETOUR-SENS1
           MOVE separator                    TO DATS20-I-RETOUR-SEPAR
           MOVE culture :: lng                 TO DATS20-I-INT-LANG
           MOVE culture :: cty               TO DATS20-I-INT-PAYS
           MOVE 'M'                          TO DATS20-I-POLICE
           MOVE 'P'                          TO DATS20-I-INJOUR
           SET DATS20-I-DATE1-SSAAMMJJ-OUI   TO TRUE

           CALL 'ZCALLPGM' USING C-ZDAT2000
                                 DATS20

           IF DATS20-O-ERREUR
               MOVE ALL '9'                  TO returnCode
               DISPLAY DATS20-O-LIBRET
           ELSE
               MOVE DATS20-O-DATE-LONG       TO Result
           END-IF
           .
       END PROGRAM currentDateFreeFormat.                                     

       END PROGRAM DVZZDAT.
