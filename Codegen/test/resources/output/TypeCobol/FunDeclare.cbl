      * 13 CodeElements errors
      * "1"@(4:8>4:16): [27:1] Syntax error : Illegal default section in library.
      * "1"@(24:8>24:14): [27:1] Syntax error : Illegal FILE SECTION in function "FunDeclare.StrangelyReturnsItsInput" declaration
      * "1"@(42:12>42:26): [27:1] Syntax error : x is already a parameter.
      * "1"@(43:12>43:26): [27:1] Syntax error : y is already a parameter.
      * "1"@(45:14>45:28): [27:1] Syntax error : x is already a parameter.
      * "1"@(46:14>46:28): [27:1] Syntax error : z is already a parameter.
      * "1"@(49:12>49:31): [27:1] Syntax error : result is already a parameter.
      * "1"@(56:12>56:27): [27:1] Syntax error : Ambiguous reference to symbol result
      * "1"@(93:14>93:34): [27:1] Syntax error : Illegal GLOBAL clause in function data item.
      * "1"@(94:14>94:36): [27:1] Syntax error : Illegal EXTERNAL clause in function data item.
      * "1"@(99:8>99:16): [27:1] Syntax error : Illegal non-function item in library
      * "1"@(112:8>112:46): [27:1] Syntax error : Condition parameter "valid-gender" must be subordinate to another parameter.
      * "1"@(112:8>112:46): [27:1] Syntax error : Condition parameter "male" must be level 88.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       
       PROCEDURE DIVISION.
            .
       
      *DECLARE function DoesNothing PUBLIC.                                   
       PROGRAM-ID. DoesNothing.                                               
         PROCEDURE DIVISION                                                   
         .                                                                    
           DISPLAY 'I DO NOTHING'
           .
       END PROGRAM DoesNothing.                                               

      *DECLARE function ReturnsZero PUBLIC.                                   
       PROGRAM-ID. ReturnsZero.                                               
         DATA DIVISION.
         LINKAGE SECTION.                                                     
           01 result PIC 9(04).                                               
         PROCEDURE DIVISION                                                   
             RETURNING result                                                 
         .                                                                    
           MOVE 0 TO result.
           .
       END PROGRAM ReturnsZero.                                               

      * ERROR Illegal FILE SECTION
      *DECLARE function StrangelyReturnsItsInput PRIVATE.                     
       PROGRAM-ID. StrangelyReturnsItsInput.                                  
         DATA DIVISION.
         FILE SECTION.
           FD myfile. 01 toto PIC X.
         LINKAGE SECTION.
           01 x PIC 9(04).                                                    
           01 result PIC 9(04).                                               
         PROCEDURE DIVISION                                                   
             USING BY REFERENCE x                                             
             RETURNING result                                                 
         .                                                                    
           IF x = 0
             MOVE 0 TO result
           ELSE
             MOVE x TO result
           END-IF.
       END PROGRAM StrangelyReturnsItsInput.                                  

      * ERROR because x,y, a.x,a.z and result shouldn't be in LINKAGE
      *DECLARE function SumThreeWithClutterInLinkage PRIVATE.                 
       PROGRAM-ID. SumThreeWithClutterInLinkage.                              
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
             RETURNING result                                                 
         .                                                                    
           MOVE 0 TO result.
           ADD x to result.
           ADD y to result.
           ADD z to result.
       END PROGRAM SumThreeWithClutterInLinkage.                              
       
      *DECLARE function SwapParameters PRIVATE.                               
       PROGRAM-ID. SwapParameters.                                            
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
       END PROGRAM SwapParameters.                                            

      * ERROR because x and y should be INOUT
      * ERROR because y INPUT vs OUTPUT types differ
      *DECLARE function SwapParametersWrong PRIVATE.                          
       PROGRAM-ID. SwapParametersWrong.                                       
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
       END PROGRAM SwapParametersWrong.                                       
      * ERROR because illegal GLOBAL or EXTERNAL
      *DECLARE function IllegalClauses PUBLIC.                                
       PROGRAM-ID. IllegalClauses.                                            
         DATA DIVISION.
           WORKING-STORAGE SECTION.
             01 x PIC X IS GLOBAL.
             01 y PIC X IS EXTERNAL.
         PROCEDURE DIVISION                                                   
         .                                                                    
           .
       END PROGRAM IllegalClauses.                                            

       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
      *DECLARE function FunConditions PRIVATE.                                
       PROGRAM-ID. FunConditions.                                             
         LINKAGE SECTION.                                                     
           01 gender PIC X(01).                                               
           88 valid-gender VALUE 'F' 'M'.                                     
           88 female VALUE 'F'.                                               
           88 male VALUE 'M'.                                                 
         PROCEDURE DIVISION                                                   
             USING BY REFERENCE gender                                        
                   BY REFERENCE valid-gender                                  
                   BY REFERENCE female                                        
                   BY REFERENCE male                                          
         .                                                                    
           CONTINUE.
       END PROGRAM FunConditions.                                             
      * ERROR level-88 parameter items must be subordinate to another item
      * ERROR only level-88 parameter items shall have an explicit level number
      *DECLARE function FunConditions PRIVATE.                                
       PROGRAM-ID. FunConditions.                                             
         LINKAGE SECTION.                                                     
           88 valid-gender VALUE 'F' 'M'.                                     
           01 gender PIC X(01).                                               
           88 female VALUE 'F'.                                               
           88 male VALUE 'M'.                                                 
         PROCEDURE DIVISION                                                   
             USING BY REFERENCE valid-gender                                  
                   BY REFERENCE gender                                        
                   BY REFERENCE female                                        
                   BY REFERENCE male                                          
         .                                                                    
           CONTINUE.
       END PROGRAM FunConditions.                                             
       
       END PROGRAM FunDeclare.
