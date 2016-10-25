      * 13 CodeElements errors
      * "1"@(4:8>4:16): [27:1] Syntax error : Illegal default section in library.
      * "1"@(27:8>27:14): [27:1] Syntax error : Illegal FILE SECTION in function "FunDeclare.StrangelyReturnsItsInput" declaration
      * "1"@(47:12>47:26): [27:1] Syntax error : x is already a parameter.
      * "1"@(48:12>48:26): [27:1] Syntax error : y is already a parameter.
      * "1"@(50:14>50:28): [27:1] Syntax error : x is already a parameter.
      * "1"@(51:14>51:28): [27:1] Syntax error : z is already a parameter.
      * "1"@(54:12>54:31): [27:1] Syntax error : result is already a parameter.
      * "1"@(56:12>56:27): [27:1] Syntax error : Ambiguous reference to symbol result
      * "1"@(93:14>93:34): [27:1] Syntax error : Illegal GLOBAL clause in function data item.
      * "1"@(94:14>94:36): [27:1] Syntax error : Illegal EXTERNAL clause in function data item.
      * "1"@(99:8>99:16): [27:1] Syntax error : Illegal non-function item in library
      * "1"@(113:8>118:14): [27:1] Syntax error : Condition parameter "valid-gender" must be subordinate to another parameter.
      * "1"@(113:8>118:10): [27:1] Syntax error : Condition parameter "male" must be level 88.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       DATA DIVISION.                                                         
       WORKING-STORAGE SECTION.                                               
       01  LibFctList-Loaded PIC X(01) VALUE SPACE.                           
           88 LibFctList-IsLoaded      VALUE '1'.                             
                                                                              
       01  LibFctList-VALUES.                                                 
      *    F0000001 -> DoesNothing                                            
           05 PIC X(08) VALUE 'F0000001'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    F0000002 -> ReturnsZero                                            
           05 PIC X(08) VALUE 'F0000002'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
      *    F0000007 -> IllegalClauses                                         
           05 PIC X(08) VALUE 'F0000007'.                                     
           05 PIC X(08) VALUE LOW-VALUES.                                     
                                                                              
       
       PROCEDURE DIVISION.
                                                                              
           SET DoesNothing TO ENTRY 'F0000001'                                
           SET ReturnsZero TO ENTRY 'F0000002'                                
           SET StrangelyReturnsItsInput TO ENTRY 'F0000003'                   
           SET SumThreeWithClutterInLinkage TO ENTRY 'F0000004'               
           SET SwapParameters TO ENTRY 'F0000005'                             
           SET SwapParametersWrong TO ENTRY 'F0000006'                        
           SET IllegalClauses TO ENTRY 'F0000007'                             
           SET FunConditions TO ENTRY 'F0000008'                              
           SET FunConditions TO ENTRY 'F0000009'                              
           SET MyNOT TO ENTRY 'F0000010'                                      
           .                                                                  
            .
       
      *DECLARE function DoesNothing PUBLIC.                                   

      *DECLARE function ReturnsZero PUBLIC                                    
      *      RETURNING result PIC 9(04).                                      

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
       
       END PROGRAM FunDeclare.
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000001.                                                  
       PROCEDURE DIVISION                                                     
           .                                                                  
           DISPLAY 'I DO NOTHING'
           .
       END PROGRAM F0000001.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000002.                                                  
         DATA DIVISION.
       LINKAGE SECTION.                                                       
       01 result PIC 9(04).                                                   
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE result                                        
           .                                                                  
           MOVE 0 TO result.
           .
       END PROGRAM F0000002.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000003.                                                  
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
       END PROGRAM F0000003.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000004.                                                  
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
       END PROGRAM F0000004.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000005.                                                  
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
       END PROGRAM F0000005.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000006.                                                  
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
       END PROGRAM F0000006.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000007.                                                  
         DATA DIVISION.
           WORKING-STORAGE SECTION.
             01 x PIC X IS GLOBAL.
             01 y PIC X IS EXTERNAL.
       PROCEDURE DIVISION                                                     
           .                                                                  
           .
       END PROGRAM F0000007.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000008.                                                  
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
       END PROGRAM F0000008.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000009.                                                  
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
       END PROGRAM F0000009.                                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. F0000010.                                                  
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
       END PROGRAM F0000010.                                                  
