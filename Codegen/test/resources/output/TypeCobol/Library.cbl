      * 14 CodeElements errors
      * "1"@(1:8>3:16): [27:1] Syntax error : Missing library copy in IDENTIFICATION DIVISION.
      * "1"@(42:8>42:16): [27:1] Syntax error : Illegal 1 USING in library PROCEDURE DIVISION.
      * "1"@(73:12>75:25): [27:1] Syntax error : Symbol DAY-OF-INTEGER is not referenced
      * "1"@(96:12>96:52): [27:1] Syntax error : Symbol DATS20-I-FONCTION-FORMATAGE is not referenced
      * "1"@(97:12>97:62): [27:1] Syntax error : Symbol DATS20-I-DATE1 is not referenced
      * "1"@(98:12>98:69): [27:1] Syntax error : Symbol DATS20-I-RETOUR-TYPE1 is not referenced
      * "1"@(99:12>99:69): [27:1] Syntax error : Symbol DATS20-I-RETOUR-SENS1 is not referenced
      * "1"@(100:12>100:69): [27:1] Syntax error : Symbol DATS20-I-RETOUR-SEPAR is not referenced
      * "1"@(101:12>101:65): [27:1] Syntax error : Symbol DATS20-I-INT-LANG is not referenced
      * "1"@(102:12>102:65): [27:1] Syntax error : Symbol DATS20-I-INT-PAYS is not referenced
      * "1"@(103:12>103:63): [27:1] Syntax error : Symbol DATS20-I-POLICE is not referenced
      * "1"@(104:12>104:63): [27:1] Syntax error : Symbol DATS20-I-INJOUR is not referenced
      * "1"@(105:12>105:52): [27:1] Syntax error : Symbol DATS20-I-DATE1-SSAAMMJJ-OUI is not referenced
      * "1"@(114:16>114:54): [27:1] Syntax error : Symbol DATS20-O-DATE-LONG is not referenced
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZDATE.
       AUTHOR. REYDELPA.
      *REMARKS. COPY=(
      *    YDATS20   YUTCDAT
      * ).

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
       01  LibFctList-Loaded PIC X(01) VALUE SPACE.                           
           88 LibFctList-IsLoaded      VALUE '1'.                             
                                                                              
       01  LibFctList-VALUES.                                                 
      *    42e5f209 -> currentDate                                            
           05 PIC X(08) VALUE '42e5f209'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    27128b8a -> currentDateDB2                                         
           05 PIC X(08) VALUE '27128b8a'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    6c4e76b4 -> currentDateJulian                                      
           05 PIC X(08) VALUE '6c4e76b4'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    d55b3ea7 -> currentDateFreeFormat                                  
           05 PIC X(08) VALUE 'd55b3ea7'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
                                                                              
       01  LibFctList REDEFINES LibFctList-Values.                            
           05   LibFctItem    OCCURS 4 INDEXED BY LibFctIndex.                
             10 LibFctCode    PIC X(08).                                      
             10 LibFctPointer PROCEDURE-POINTER.                              
      *_________________________________________________________________
       LINKAGE SECTION.
       01  FctList.                                                           
           05 NumberOfFunctions   PIC 9(04).                                  
           05 FctItem OCCURS 9999 DEPENDING ON NumberOfFunctions              
                                  INDEXED BY FctIndex.                        
             10 FctCode    PIC X(08).                                         
             10 FctPointer PROCEDURE-POINTER VALUE NULL.                      
           COPY ?TCRFUN_LIBRARY_COPY? REPLACING ==:?TCRFUN_LIBRARY_COPY?      
      -:== BY ==FCT==.                                                        
       01  CallData.                                                          
           05  DescriptionId PIC X(08).                                       
             88 CallIsCopy VALUE '?TCRFUN_LIBRARY_COPY?'.                     
           COPY YDVZDAT REPLACING ==:DVZDAT:== BY ==DVZDAT==.

      *=================================================================
      *PROCEDURE DIVISION USING DVZDAT.                                       
       PROCEDURE DIVISION USING CallData.                                     
           IF CallIsCopy                                                      
             PERFORM Copy-Process-Mode                                        
           ELSE                                                               
             PERFORM FctList-Process-Mode                                     
           END-IF                                                             
                                                                              
           GOBACK                                                             
           .                                                                  
      *=================================================================
      *DECLARE FUNCTION currentDate PUBLIC                                    
      *    RETURNING Result TYPE date.                                        
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateDB2 PUBLIC                                 
      *    RETURNING Result Type dateDB2.                                     


      *_________________________________________________________________
      *DECLARE FUNCTION currentDateJulian PUBLIC                              
      *    RETURNING Result Type dateJulian.                                  


      *_________________________________________________________________
      *DECLARE FUNCTION currentDateFreeFormat PUBLIC                          
      *                   INPUT dateType   PIC X(01)                          
      *                         direction  PIC X(01)                          
      *                         separator  PIC X(01)                          
      *                         culture    TYPE culture                       
      *                         returnCode PIC 9(04)                          
      *                   RETURNING Result PIC X(40).                         
                                                                              
       Copy-Process-Mode.                                                     
           SET ADDRESS OF FCT TO ADDRESS OF CallData                          
                                                                              
           SET FCT-currentDate-01   TO ENTRY '42e5f209'                       
           SET FCT-currentDateDB2-01   TO ENTRY '27128b8a'                    
           SET FCT-currentDateJulian-01   TO ENTRY '6c4e76b4'                 
           SET FCT-currentDateFreeFormat-01   TO ENTRY 'd55b3ea7'             
           .                                                                  
                                                                              
       FctList-Process-Mode.                                                  
           SET ADDRESS OF FctList TO ADDRESS OF CallData                      
                                                                              
           IF NOT LibFctList-IsLoaded                                         
             SET LibFctPointer(1)   TO ENTRY '42e5f209'                       
             SET LibFctPointer(2)   TO ENTRY '27128b8a'                       
             SET LibFctPointer(3)   TO ENTRY '6c4e76b4'                       
             SET LibFctPointer(4)   TO ENTRY 'd55b3ea7'                       
                                                                              
             SET LibFctList-IsLoaded TO TRUE                                  
           END-IF                                                             
                                                                              
           PERFORM VARYING FctIndex FROM 1 BY 1                               
                   UNTIL FctIndex > NumberOfFunctions                         
                                                                              
             SEARCH LibFctItem VARYING LibFctIndex                            
               WHEN LibFctCode(LibFctIndex) = FctCode(FctIndex)               
                 SET FctPointer(FctIndex) TO LibFctPointer(LibFctIndex)       
             END-SEARCH                                                       
                                                                              
           END-PERFORM                                                        
           .                                                                  






       END PROGRAM DVZZDAT.
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. 42e5f209.                                                  
       DATA DIVISION.                                                         
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
       END PROGRAM 42e5f209.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. 27128b8a.                                                  
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
       END PROGRAM 27128b8a.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. 6c4e76b4.                                                  
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
       END PROGRAM 6c4e76b4.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. d55b3ea7.                                                  
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
      *    MOVE culture :: lng               TO DATS20-I-INT-LANG             
           MOVE lng OF culture               TO DATS20-I-INT-LANG             
      *    MOVE culture :: cty               TO DATS20-I-INT-PAYS             
           MOVE cty OF culture               TO DATS20-I-INT-PAYS             
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
       END PROGRAM d55b3ea7.                                                  
