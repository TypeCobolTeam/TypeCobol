      * 3 CodeElements errors
      * "1"@(44:12>44:26): [27:1] Syntax error : a is not a parameter.
      * "1"@(45:12>45:26): [27:1] Syntax error : b is not a parameter.
      * "1"@(46:12>46:26): [27:1] Syntax error : c is not a parameter.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       
       PROCEDURE DIVISION.
       
      *DECLARE DoesNothing PUBLIC.                                            
       PROGRAM-ID. DoesNothing.                                               
         PROCEDURE DIVISION                                                   
         .                                                                    
           DISPLAY 'I DO NOTHING'
           .
       END PROGRAM DoesNothing.                                               

      *DECLARE ReturnsZero PUBLIC.                                            
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
       
      *DECLARE StrangelyReturnsItsInput PRIVATE.                              
       PROGRAM-ID. StrangelyReturnsItsInput.                                  
         DATA DIVISION.
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
       
      *DECLARE SumThreeWithClutterInLinkage PRIVATE.                          
       PROGRAM-ID. SumThreeWithClutterInLinkage.                              
         DATA DIVISION.
         LINKAGE SECTION.
           01 x PIC 9(04).
           01 y PIC 9(04).
           01 z PIC 9(04).
           01 a PIC 9(04).
           01 b PIC 9(04).
           01 c PIC 9(04).
           01 result PIC 9(04).
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
       
      *DECLARE SwapParameters PRIVATE.                                        
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
             RETURNING x                                                      
         .                                                                    
           MOVE x TO tmp
           MOVE y TO x
           MOVE tmp TO y
           .
       END PROGRAM SwapParameters.                                            
       
       END PROGRAM FunDeclare.
       
