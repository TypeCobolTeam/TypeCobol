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

      *01  dateJulian    TYPEDEF STRONG.                                      
      *    10 YYYY                   PIC 9(04).                               
      *    10 DDD                    PIC 9(03).                               

      *01  dateDB2       TYPEDEF STRONG.                                      
      *    10 YYYY                   PIC 9(04).                               
      *    10                        PIC X(01).                               
      *    10 MM                     PIC 9(02).                               
      *    10                        PIC X(01).                               
      *    10 DD                     PIC 9(02).                               

      *01  dateString    TYPEDEF     PIC 9(08).                               

      *01 culture        TYPEDEF STRONG.                                      
      *    10 lng                    PIC X(02).                               
      *    10 cty                    PIC X(02).                               
       01  LibFctList-Loaded PIC X(01) VALUE SPACE.                           
           88 LibFctList-IsLoaded      VALUE '1'.                             
       01  LibFctList-VALUES.                                                 
      *    e5f209fa -> currentDate                                            
           05 PIC X(08) VALUE 'e5f209fa'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    b8ac0397 -> currentDateDB2                                         
           05 PIC X(08) VALUE 'b8ac0397'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    c4e76b45 -> currentDateJulian                                      
           05 PIC X(08) VALUE 'c4e76b45'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    d55b3ea7 -> currentDateFreeFormat                                  
           05 PIC X(08) VALUE 'd55b3ea7'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    bfb0fa9b -> currentDateString                                      
           05 PIC X(08) VALUE 'bfb0fa9b'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
                                                                              
       01  LibFctList REDEFINES LibFctList-Values.                            
           05   LibFctItem    OCCURS 5 INDEXED BY LibFctIndex.                
             10 LibFctCode    PIC X(08).                                      
             10 LibFctPointer PROCEDURE-POINTER.                              
       LINKAGE SECTION.                                                       
       01  FctList.                                                           
           05 NumberOfFunctions   PIC 9(04).                                  
           05 FctItem OCCURS 9999 DEPENDING ON NumberOfFunctions              
                                  INDEXED BY FctIndex.                        
             10 FctCode    PIC X(08).                                         
             10 FctPointer PROCEDURE-POINTER VALUE NULL.                      
       01  CallData.                                                          
           05  DescriptionId PIC X(08).                                       
             88 CallIsCopy VALUE 'CALL FROM COBOL NOT SUPPORTED'.             

      *=================================================================
      *PROCEDURE DIVISION.                                                    
       PROCEDURE DIVISION USING CallData.
                          
      *=================================================================
      *DECLARE FUNCTION currentDate PUBLIC                                    
      *Description of currentDate                                             
      *    RETURNING Result TYPE date.                                        
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateDB2 PUBLIC                                 
      *    RETURNING Result Type dateDB2.                                     
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateJulian PUBLIC                              
      * my comment                                                            
      *    RETURNING Result Type dateJulian.                                  
      *_________________________________________________________________
      *DECLARE FUNCTION currentDateFreeFormat PUBLIC                          
      *                   INPUT dateType   PIC X(01)                          
      *                         direction  PIC X(01)                          
      *                         separator  PIC X(01)                          
      *                         culture    TYPE culture                       
      *                         returnCode PIC 9(04)                          
      *                   RETURNING Result PIC X(40).                         
      *_________________________________________________________________
      *Keep spaces at end of line, because there were 
      * presents in source file                      
      *DECLARE FUNCTION currentDateString PUBLIC                              
      *    RETURNING Result TYPE dateString.                                  

       IF CallIsCopy                                                          
             PERFORM Copy-Process-Mode                                        
           ELSE                                                               
             PERFORM FctList-Process-Mode                                     
           END-IF                                                             
                                                                              
           GOBACK                                                             
           .                                                                  
       Copy-Process-Mode.                                                     
           SET ADDRESS OF FCT TO ADDRESS OF CallData                          
                                                                              
           SET FCT-currentDate-01   TO ENTRY 'e5f209fa'                       
           SET FCT-currentDateDB2-01   TO ENTRY 'b8ac0397'                    
           SET FCT-currentDateJulian-01   TO ENTRY 'c4e76b45'                 
           SET FCT-currentDateFreeFormat-01   TO ENTRY 'd55b3ea7'             
           SET FCT-currentDateString-01   TO ENTRY 'bfb0fa9b'                 
           .                                                                  
       FctList-Process-Mode.                                                  
           SET ADDRESS OF FctList TO ADDRESS OF CallData                      
                                                                              
           IF NOT LibFctList-IsLoaded                                         
             SET LibFctPointer(1)   TO ENTRY 'e5f209fa'                       
             SET LibFctPointer(2)   TO ENTRY 'b8ac0397'                       
             SET LibFctPointer(3)   TO ENTRY 'c4e76b45'                       
             SET LibFctPointer(4)   TO ENTRY 'd55b3ea7'                       
             SET LibFctPointer(5)   TO ENTRY 'bfb0fa9b'                       
                                                                              
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
      *                                                                       
      *DECLARE FUNCTION currentDate PUBLIC                                    
      *Description of currentDate                                             
      *    RETURNING Result TYPE date.                                        
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. e5f209fa.                                                  
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
       END PROGRAM e5f209fa.                                                  
      *                                                                       
      *DECLARE FUNCTION currentDateDB2 PUBLIC                                 
      *    RETURNING Result Type dateDB2.                                     
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. b8ac0397.                                                  
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
      *    move W-Dat :: YYYY       to Result :: YYYY                         
           move W-Dat :: YYYY       to YYYY  OF  Result
      *    move W-Dat :: MM         to Result :: MM                           
           move W-Dat :: MM         to MM  OF  Result
      *    move W-Dat :: DD         to Result :: DD                           
           move W-Dat :: DD         to DD  OF  Result 
      *    move '-'                 to Result(5:1)
      *    move '-'                 to Result(8:1)
           
           .
       END PROGRAM b8ac0397.                                                  
      *                                                                       
      *DECLARE FUNCTION currentDateJulian PUBLIC                              
      * my comment                                                            
      *    RETURNING Result Type dateJulian.                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. c4e76b45.                                                  
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
      *    move unsafe W-Dat to Result                                        
           move        W-Dat to Result
                                      
           .
       END PROGRAM c4e76b45.                                                  
      *                                                                       
      *DECLARE FUNCTION currentDateFreeFormat PUBLIC                          
      *                   INPUT dateType   PIC X(01)                          
      *                         direction  PIC X(01)                          
      *                         separator  PIC X(01)                          
      *                         culture    TYPE culture                       
      *                         returnCode PIC 9(04)                          
      *                   RETURNING Result PIC X(40).                         
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. d55b3ea7.                                                  
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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


           continue
           .
       END PROGRAM d55b3ea7.                                                  
      *                                                                       
      *DECLARE FUNCTION currentDateString PUBLIC                              
      *    RETURNING Result TYPE dateString.                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. bfb0fa9b.                                                  
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 Result PIC 9(08).                                                   
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE Result                                        
           .                                                                  
           ACCEPT Result FROM DATE YYYYMMDD
           .
       END PROGRAM bfb0fa9b.                                                  
