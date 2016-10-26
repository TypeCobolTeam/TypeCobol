      * 13 CodeElements errors
      * "1"@(5:8>5:16): [27:1] Syntax error : Illegal default section in library.
      * "1"@(36:8>36:14): [27:1] Syntax error : Illegal FILE SECTION in function "FunDeclare.StrangelyReturnsItsInput" declaration
      * "1"@(56:12>56:26): [27:1] Syntax error : x is already a parameter.
      * "1"@(57:12>57:26): [27:1] Syntax error : y is already a parameter.
      * "1"@(59:14>59:28): [27:1] Syntax error : x is already a parameter.
      * "1"@(60:14>60:28): [27:1] Syntax error : z is already a parameter.
      * "1"@(63:12>63:31): [27:1] Syntax error : result is already a parameter.
      * "1"@(65:12>65:27): [27:1] Syntax error : Ambiguous reference to symbol result
      * "1"@(102:14>102:34): [27:1] Syntax error : Illegal GLOBAL clause in function data item.
      * "1"@(103:14>103:36): [27:1] Syntax error : Illegal EXTERNAL clause in function data item.
      * "1"@(108:8>108:16): [27:1] Syntax error : Illegal non-function item in library
      * "1"@(122:8>127:14): [27:1] Syntax error : Condition parameter "valid-gender" must be subordinate to another parameter.
      * "1"@(122:8>127:10): [27:1] Syntax error : Condition parameter "male" must be level 88.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
      *SERVICE IS YFUNCOPY.
       DATA DIVISION.                                                         
       WORKING-STORAGE SECTION.                                               
       01  LibFctList-Loaded PIC X(01) VALUE SPACE.                           
           88 LibFctList-IsLoaded      VALUE '1'.                             
                                                                              
       01  LibFctList-VALUES.                                                 
      *    05a59b2c -> DoesNothing                                            
           05 PIC X(08) VALUE '05a59b2c'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    8fe03398 -> DoesNothing                                            
           05 PIC X(08) VALUE '8fe03398'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    9072a866 -> ReturnsZero                                            
           05 PIC X(08) VALUE '9072a866'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    e6215ae7 -> IllegalClauses                                         
           05 PIC X(08) VALUE 'e6215ae7'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
                                                                              
       01  LibFctList REDEFINES LibFctList-Values.                            
           05   LibFctItem    OCCURS 4 INDEXED BY LibFctIndex.                
             10 LibFctCode    PIC X(08).                                      
             10 LibFctPointer PROCEDURE-POINTER.                              
       LINKAGE SECTION.                                                       
       01  FctList.                                                           
           05 NumberOfFunctions   PIC 9(04).                                  
           05 FctItem OCCURS 9999 DEPENDING ON NumberOfFunctions              
                                  INDEXED BY FctIndex.                        
             10 FctCode    PIC X(08).                                         
             10 FctPointer PROCEDURE-POINTER VALUE NULL.                      
           COPY YFUNCOPY REPLACING ==:YFUNCOPY:== BY ==FCT==.                 
       01  CallData.                                                          
           05  DescriptionId PIC X(08).                                       
             88 CallIsCopy VALUE 'YFUNCOPY'.                                  
       
      *PROCEDURE DIVISION.                                                    
       PROCEDURE DIVISION USING CallData.                                     
           IF CallIsCopy                                                      
             PERFORM Copy-Process-Mode                                        
           ELSE                                                               
             PEFORM FctList-Process-Mode                                      
           END-IF                                                             
                                                                              
           GOBACK                                                             
           .
       
      *DECLARE function DoesNothing PUBLIC.                                   

      *DECLARE function ReturnsZero PUBLIC                                    
      *      RETURNING result PIC 9(04).                                      

      *OK: second function with same name, but profile is different
      *DECLARE function DoesNothing PUBLIC                                    
      *      INPUT x PIC 9(04).                                               

      * ERROR Illegal FILE SECTION
      *DECLARE function StrangelyReturnsItsInput PRIVATE                      
      *      INPUT     x      PIC 9(04)                                       
      *      RETURNING result PIC 9(04)                                       
      *  .                                                                    

      * ERROR because x,y, a.x,a.z and result shouldn't be in LINKAGE
      *DECLARE function SumThreeWithClutterInLinkage PRIVATE                  
      *      INPUT x PIC 9(04)                                                
      *            y PIC 9(04)                                                
      *            z PIC 9(04)                                                
      *      RETURNING result PIC 9(04)                                       
      *  .                                                                    
       
      *DECLARE function SwapParameters PRIVATE                                
      *      INOUT x PIC 9(04)                                                
      *            y PIC 9(04)                                                
      *  .                                                                    

      * ERROR because x and y should be INOUT
      * ERROR because y INPUT vs OUTPUT types differ
      *DECLARE function SwapParametersWrong PRIVATE                           
      *      INPUT  x PIC 9(04)                                               
      *             y PIC 9(04)                                               
      *             a PIC 9(04)                                               
      *      OUTPUT x PIC 9(04)                                               
      *             y PIC 9(08)                                               
      *             b PIC 9(04)                                               
      *  .                                                                    
      * ERROR because illegal GLOBAL or EXTERNAL
      *DECLARE function IllegalClauses PUBLIC.                                

       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
      *DECLARE function FunConditions PRIVATE                                 
      *      INPUT  gender PIC X(01)                                          
      *          88  valid-gender VALUE 'F' 'M'                               
      *          88  female VALUE 'F'                                         
      *          88  male   VALUE 'M'                                         
      *  .                                                                    
      * ERROR level-88 parameter items must be subordinate to another item
      * ERROR only level-88 parameter items shall have an explicit level number
      *DECLARE function FunConditions PRIVATE                                 
      *      INPUT 88 valid-gender VALUE 'F' 'M'                              
      *               gender PIC X(01)                                        
      *            88  female VALUE 'F'                                       
      *            01  male   VALUE 'M'                                       
      *  .                                                                    
       
      *DECLARE FUNCTION MyNOT PRIVATE                                         
      *      INPUT     x type BOOL                                            
      *      RETURNING y TYPE bool                                            
      *  .                                                                    
                                                                              
       Copy-Process-Mode.                                                     
           SET ADDRESS OF FCT TO ADDRESS OF CallData                          
                                                                              
           SET FCT-DoesNothing   TO ENTRY '05a59b2c'                          
           SET FCT-DoesNothing   TO ENTRY '8fe03398'                          
           SET FCT-ReturnsZero   TO ENTRY '9072a866'                          
           SET FCT-IllegalClauses   TO ENTRY 'e6215ae7'                       
           .                                                                  
                                                                              
       FctList-Process-Mode.                                                  
           SET ADDRESS OF FctList TO ADDRESS OF CallData                      
                                                                              
           IF NOT LibFctList-IsLoaded                                         
             SET LibFctPointer(1)   TO ENTRY '05a59b2c'                       
             SET LibFctPointer(2)   TO ENTRY '8fe03398'                       
             SET LibFctPointer(3)   TO ENTRY '9072a866'                       
             SET LibFctPointer(4)   TO ENTRY 'e6215ae7'                       
                                                                              
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
       
       END PROGRAM FunDeclare.
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. 05a59b2c.                                                  
       PROCEDURE DIVISION                                                     
           .                                                                  
           DISPLAY 'I DO NOTHING'
           .
       END PROGRAM 05a59b2c.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. 9072a866.                                                  
         DATA DIVISION.
       LINKAGE SECTION.                                                       
       01 result PIC 9(04).                                                   
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE result                                        
           .                                                                  
           MOVE 0 TO result.
           .
       END PROGRAM 9072a866.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. 8fe03398.                                                  
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 x PIC 9(04).                                                        
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x                                             
           .                                                                  
           DISPLAY 'I DO NOTHING WITH ' x
           .
       END PROGRAM 8fe03398.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000004.                                                  
         DATA DIVISION.
         FILE SECTION.
           FD myfile. 01 toto PIC X.
         LINKAGE SECTION.
       01 x PIC 9(04).                                                        
       01 result PIC 9(04).                                                   
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x                                             
                   BY REFERENCE result                                        
           .                                                                  
           IF x = 0
             MOVE 0 TO result
           ELSE
             MOVE x TO result
           END-IF.
       END PROGRAM F0000004.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000005.                                                  
         DATA DIVISION.
         LINKAGE SECTION.
           01 x PIC 9(04).
           01 y PIC 9(02).
           01 a PIC 9(04).
             05 x PIC 9(02).
             05 z PIC 9(02).
           01 b PIC 9(04).
           01 c PIC 9(04).
           01 result PIC 9(04).
       01 z PIC 9(04).                                                        
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x                                             
                   BY REFERENCE y                                             
                   BY REFERENCE z                                             
                   BY REFERENCE result                                        
           .                                                                  
           MOVE 0 TO result.
           ADD x to result.
           ADD y to result.
           ADD z to result.
       END PROGRAM F0000005.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000006.                                                  
         DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 tmp PIC 9(04).
       LINKAGE SECTION.                                                       
       01 x PIC 9(04).                                                        
       01 y PIC 9(04).                                                        
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x                                             
                   BY REFERENCE y                                             
           .                                                                  
           MOVE x TO tmp
           MOVE y TO x
           MOVE tmp TO y
           .
       END PROGRAM F0000006.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000007.                                                  
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 x PIC 9(04).                                                        
       01 y PIC 9(04).                                                        
       01 a PIC 9(04).                                                        
       01 b PIC 9(04).                                                        
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x                                             
                   BY REFERENCE y                                             
                   BY REFERENCE a                                             
                   BY REFERENCE b                                             
           .                                                                  
           CONTINUE.
       END PROGRAM F0000007.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. e6215ae7.                                                  
         DATA DIVISION.
           WORKING-STORAGE SECTION.
             01 x PIC X IS GLOBAL.
             01 y PIC X IS EXTERNAL.
       PROCEDURE DIVISION                                                     
           .                                                                  
           .
       END PROGRAM e6215ae7.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000009.                                                  
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 gender PIC X(01).                                                   
           88 valid-gender VALUE 'F' 'M'.                                     
           88 female VALUE 'F'.                                               
           88 male VALUE 'M'.                                                 
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE gender                                        
           .                                                                  
           CONTINUE.
       END PROGRAM F0000009.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000010.                                                  
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 valid-gender.                                                       
       01 gender PIC X(01).                                                   
           88 female VALUE 'F'.                                               
           88 male VALUE 'M'.                                                 
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE gender                                        
           .                                                                  
           CONTINUE.
       END PROGRAM F0000010.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000011.                                                  
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 x PIC X     VALUE LOW-VALUE.                                        
           88 x       VALUE 'T'.                                              
           88 x-false VALUE 'F'.                                              
       01 y PIC X     VALUE LOW-VALUE.                                        
           88 y       VALUE 'T'.                                              
           88 y-false VALUE 'F'.                                              
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x                                             
                   BY REFERENCE y                                             
           .                                                                  
           IF NOT x
             SET y TO TRUE
           ELSE
      *      SET y TO FALSE                                                   
               SET y-false TO TRUE.                                           
           END-IF.
       END PROGRAM F0000011.                                                  
