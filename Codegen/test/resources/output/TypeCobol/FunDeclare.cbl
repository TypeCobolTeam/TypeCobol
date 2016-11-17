      * 13 CodeElements errors
      * "1"@(9:8>9:16): [27:1] Syntax error : Illegal default section in library.
      * "1"@(40:8>40:14): [27:1] Syntax error : Illegal FILE SECTION in function "FunDeclare.StrangelyReturnsItsInput" declaration
      * "1"@(60:12>60:26): [27:1] Syntax error : x is already a parameter.
      * "1"@(61:12>61:26): [27:1] Syntax error : y is already a parameter.
      * "1"@(63:14>63:28): [27:1] Syntax error : x is already a parameter.
      * "1"@(64:14>64:28): [27:1] Syntax error : z is already a parameter.
      * "1"@(67:12>67:31): [27:1] Syntax error : result is already a parameter.
      * "1"@(69:12>69:27): [27:1] Syntax error : Ambiguous reference to symbol result
      * "1"@(106:14>106:34): [27:1] Syntax error : Illegal GLOBAL clause in function data item.
      * "1"@(107:14>107:36): [27:1] Syntax error : Illegal EXTERNAL clause in function data item.
      * "1"@(112:8>112:16): [27:1] Syntax error : Illegal non-function item in library
      * "1"@(126:8>131:14): [27:1] Syntax error : Condition parameter "valid-gender" must be subordinate to another parameter.
      * "1"@(126:8>131:10): [27:1] Syntax error : Condition parameter "male" must be level 88.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
      *SERVICE IS YFUNCOPY.                                                   
       
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.                                               
       01  LibFctList-Loaded PIC X(01) VALUE SPACE.                           
           88 LibFctList-IsLoaded      VALUE '1'.                             
                                                                              
       01  LibFctList-VALUES.                                                 
      *    a59b2c49 -> DoesNothing                                            
           05 PIC X(08) VALUE 'a59b2c49'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    fe03398a -> DoesNothing                                            
           05 PIC X(08) VALUE 'fe03398a'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    a866b35c -> ReturnsZero                                            
           05 PIC X(08) VALUE 'a866b35c'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    e3e490ae -> StrangelyReturnsItsInput                               
           05 PIC X(08) VALUE 'e3e490ae'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    c98a2036 -> SumThreeWithClutterInLinkage                           
           05 PIC X(08) VALUE 'c98a2036'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    e6215ae7 -> IllegalClauses                                         
           05 PIC X(08) VALUE 'e6215ae7'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
                                                                              
       01  LibFctList REDEFINES LibFctList-Values.                            
           05   LibFctItem    OCCURS 6 INDEXED BY LibFctIndex.                
             10 LibFctCode    PIC X(08).                                      
             10 LibFctPointer PROCEDURE-POINTER.                              
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.                                                       
               COPY YFUNCOPY REPLACING ==:YFUNCOPY:== BY ==FCT==.             
       01  CallData.                                                          
           05  DescriptionId PIC X(08).                                       
             88 CallIsCopy VALUE 'YFUNCOPY'.                                  
       01  FctList.                                                           
           05 NumberOfFunctions   PIC 9(04).                                  
           05 FctItem OCCURS 9999 DEPENDING ON NumberOfFunctions              
                                  INDEXED BY FctIndex.                        
             10 FctCode    PIC X(08).                                         
             10 FctPointer PROCEDURE-POINTER VALUE NULL.                      

      *PROCEDURE DIVISION.                                                    
       PROCEDURE DIVISION USING CallData.                                     
           IF CallIsCopy                                                      
             PERFORM Copy-Process-Mode                                        
           ELSE                                                               
             PERFORM FctList-Process-Mode                                     
           END-IF                                                             
                                                                              
           GOBACK                                                             
           .
       
      *DECLARE FUNCTION DoesNothing PUBLIC.                                   

      *DECLARE FUNCTION ReturnsZero PUBLIC                                    
      *      RETURNING result PIC 9(04).                                      

      *OK: second function with same name, but profile is different
      *DECLARE FUNCTION DoesNothing PUBLIC                                    
      *      INPUT x PIC 9(04).                                               

      * ERROR Illegal FILE SECTION
      *DECLARE FUNCTION StrangelyReturnsItsInput PUBLIC                       
      *      INPUT     x      PIC 9(04)                                       
      *      RETURNING result PIC 9(04)                                       
      *  .                                                                    

      * ERROR because x,y, a.x,a.z and result shouldn't be in LINKAGE
      *DECLARE FUNCTION SumThreeWithClutterInLinkage PUBLIC                   
      *      INPUT x PIC 9(04)                                                
      *            y PIC 9(04)                                                
      *            z PIC 9(04)                                                
      *      RETURNING result PIC 9(04)                                       
      *  .                                                                    
       
      *DECLARE PROCEDURE SwapParameters PRIVATE                               
      *      INOUT x PIC 9(04)                                                
      *            y PIC 9(04)                                                
      *  .                                                                    

      * ERROR because x and y should be INOUT
      * ERROR because y INPUT vs OUTPUT types differ
      *DECLARE PROCEDURE SwapParametersWrong PRIVATE                          
      *      INPUT  x PIC 9(04)                                               
      *             y PIC 9(04)                                               
      *             a PIC 9(04)                                               
      *      OUTPUT x PIC 9(04)                                               
      *             y PIC 9(08)                                               
      *             b PIC 9(04)                                               
      *  .                                                                    
      * ERROR because illegal GLOBAL or EXTERNAL
      *DECLARE FUNCTION IllegalClauses PUBLIC.                                

       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
      *DECLARE FUNCTION FunConditions PRIVATE                                 
      *      INPUT  gender PIC X(01)                                          
      *          88  valid-gender VALUE 'F' 'M'                               
      *          88  female VALUE 'F'                                         
      *          88  male   VALUE 'M'                                         
      *  .                                                                    
      * ERROR level-88 parameter items must be subordinate to another item
      * ERROR only level-88 parameter items shall have an explicit level number
      *DECLARE FUNCTION FunConditions PRIVATE                                 
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
                                                                              
           SET FCT-DoesNothing-01   TO ENTRY 'a59b2c49'                       
           SET FCT-DoesNothing-02   TO ENTRY 'fe03398a'                       
           SET FCT-ReturnsZero-01   TO ENTRY 'a866b35c'                       
           SET FCT-StrangelyReturnsItsInput-01   TO ENTRY 'e3e490ae'          
           SET FCT-SumThreeWithClutterInLinkag-01   TO ENTRY 'c98a2036'       
           SET FCT-IllegalClauses-01   TO ENTRY 'e6215ae7'                    
           .                                                                  
                                                                              
       FctList-Process-Mode.                                                  
           SET ADDRESS OF FctList TO ADDRESS OF CallData                      
                                                                              
           IF NOT LibFctList-IsLoaded                                         
             SET LibFctPointer(1)   TO ENTRY 'a59b2c49'                       
             SET LibFctPointer(2)   TO ENTRY 'fe03398a'                       
             SET LibFctPointer(3)   TO ENTRY 'a866b35c'                       
             SET LibFctPointer(4)   TO ENTRY 'e3e490ae'                       
             SET LibFctPointer(5)   TO ENTRY 'c98a2036'                       
             SET LibFctPointer(6)   TO ENTRY 'e6215ae7'                       
                                                                              
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
       PROGRAM-ID. a59b2c49.                                                  
       PROCEDURE DIVISION                                                     
           .                                                                  
           DISPLAY 'I DO NOTHING'
           .
       END PROGRAM a59b2c49.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. a866b35c.                                                  
         DATA DIVISION.
       LINKAGE SECTION.                                                       
       01 result PIC 9(04).                                                   
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE result                                        
           .                                                                  
           MOVE 0 TO result.
           .
       END PROGRAM a866b35c.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. fe03398a.                                                  
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 x PIC 9(04).                                                        
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x                                             
           .                                                                  
           DISPLAY 'I DO NOTHING WITH ' x
           .
       END PROGRAM fe03398a.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. e3e490ae.                                                  
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
       END PROGRAM e3e490ae.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. c98a2036.                                                  
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
       END PROGRAM c98a2036.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. ceb46e19.                                                  
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
       END PROGRAM ceb46e19.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. f6e9c448.                                                  
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
       END PROGRAM f6e9c448.                                                  
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
       PROGRAM-ID. b8045acd.                                                  
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
       END PROGRAM b8045acd.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. f6c8f8b0.                                                  
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
       END PROGRAM f6c8f8b0.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. ca65c9a2.                                                  
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 x-value PIC X     VALUE LOW-VALUE.                                  
           88 x       VALUE 'T'.                                              
           88 x-false VALUE 'F'.                                              
       01 y-value PIC X     VALUE LOW-VALUE.                                  
           88 y       VALUE 'T'.                                              
           88 y-false VALUE 'F'.                                              
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x-value                                       
                   BY REFERENCE y-value                                       
           .                                                                  
           IF NOT x
             SET y TO TRUE
           ELSE
      *      SET y TO FALSE                                                   
               SET y-false TO TRUE                                            
           END-IF.
       END PROGRAM ca65c9a2.                                                  
