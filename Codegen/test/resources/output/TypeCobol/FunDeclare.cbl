      * 5 CodeElements errors
      * "1"@(4:8>4:16): [27:1] Syntax error : Illegal default section in library.
      * "1"@(46:12>46:26): [27:1] Syntax error : a is not a parameter.
      * "1"@(47:12>47:26): [27:1] Syntax error : b is not a parameter.
      * "1"@(48:12>48:26): [27:1] Syntax error : c is not a parameter.
      * "1"@(77:8>77:16): [27:1] Syntax error : Illegal non-function item in library
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       
       PROCEDURE DIVISION.
      *code généré pour gérer les pointeurs
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
       
      *DECLARE function StrangelyReturnsItsInput PRIVATE.                     
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
       
      *DECLARE function SumThreeWithClutterInLinkage PRIVATE.                 
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
             RETURNING x                                                      
         .                                                                    
           MOVE x TO tmp
           MOVE y TO x
           MOVE tmp TO y
           .
       END PROGRAM SwapParameters.                                            

       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
       END PROGRAM FunDeclare.
       
