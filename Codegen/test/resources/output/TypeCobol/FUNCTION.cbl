      * 7 CodeElements errors
      * "1"@(17:12>17:36): [27:1] Syntax error : Function POW is missing parameter 2 of type Numeric
      * "1"@(18:12>18:40): [27:1] Syntax error : Function POW only takes 2 parameters
      * "1"@(20:12>20:38): [27:1] Syntax error : Symbol i is not referenced
      * "1"@(20:12>20:38): [27:1] Syntax error : Symbol j is not referenced
      * "1"@(22:12>22:38): [27:1] Syntax error : Function POW expected parameter 2 of type Numeric (actual: BOOL)
      * "1"@(24:12>24:38): [27:1] Syntax error : Function POW expected parameter 2 of max length 3 (actual: 5)
      * "1"@(26:12>26:41): [27:1] Syntax error : Symbol POWAAA is not referenced
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Functions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  x PIC 9.
       01  y PIC 9(3).
       01  z PIC 9(5).
       01  b TYPE BOOL.
       01 TC-DEFAULTcpy COPY TC-DEFAULTcpy.                                   
       01 TC-DEFAULT PIC X(08) VALUE 'TC-DEFAULT'.                            
       01 RETURN-CODE PIC X(08).                                              
       01 POW-RESULT PIC X(8).                                                

       PROCEDURE DIVISION.

       TRAITEMENT.
                                                                              
       IF TC-DEFAULTcpy-POINTER-TABLE = LOW_VALUE                             
           CALL TC-DEFAULT USING TC-DEFAULTcpy                                
       END-IF                                                                 
                                                                              
                                                                              
       CALL POW USING                                                         
           BY REFERENCE x                                                     
           BY REFERENCE y                                                     
                                                                              
           BY REFERENCE RETURN-CODE                                           
           BY REFERENCE POW-RESULT                                            
                                                                              
       IF RETURN-CODE = ZERO                                                  
           MOVE POW-RESULT TO x                                               
       ELSE                                                                   
      *    TODO: error management                                             
       END-IF                                                                 
                                                                              
                                                                              
       CALL POW USING                                                         
           BY REFERENCE y                                                     
           BY CONTENT SPACE                                                   
                                                                              
           BY REFERENCE RETURN-CODE                                           
           BY REFERENCE POW-RESULT                                            
                                                                              
       IF RETURN-CODE = ZERO                                                  
           MOVE POW-RESULT TO x                                               
       ELSE                                                                   
      *    TODO: error management                                             
       END-IF                                                                 
                                                                              
                                                                              
       CALL POW USING                                                         
           BY REFERENCE x                                                     
           BY REFERENCE y                                                     
                                                                              
           BY REFERENCE RETURN-CODE                                           
           BY REFERENCE POW-RESULT                                            
                                                                              
       IF RETURN-CODE = ZERO                                                  
           MOVE POW-RESULT TO x                                               
       ELSE                                                                   
      *    TODO: error management                                             
       END-IF                                                                 
                                                                              
                                                                              
       CALL POW USING                                                         
           BY REFERENCE i                                                     
           BY REFERENCE j                                                     
                                                                              
           BY REFERENCE RETURN-CODE                                           
           BY REFERENCE POW-RESULT                                            
                                                                              
       IF RETURN-CODE = ZERO                                                  
           MOVE POW-RESULT TO x                                               
       ELSE                                                                   
      *    TODO: error management                                             
       END-IF                                                                 
                                                                              
                                                                              
       CALL POW USING                                                         
           BY REFERENCE x                                                     
           BY REFERENCE b                                                     
                                                                              
           BY REFERENCE RETURN-CODE                                           
           BY REFERENCE POW-RESULT                                            
                                                                              
       IF RETURN-CODE = ZERO                                                  
           MOVE POW-RESULT TO x                                               
       ELSE                                                                   
      *    TODO: error management                                             
       END-IF                                                                 
                                                                              
                                                                              
       CALL POW USING                                                         
           BY REFERENCE z                                                     
           BY REFERENCE z                                                     
                                                                              
           BY REFERENCE RETURN-CODE                                           
           BY REFERENCE POW-RESULT                                            
                                                                              
       IF RETURN-CODE = ZERO                                                  
           MOVE POW-RESULT TO x                                               
       ELSE                                                                   
      *    TODO: error management                                             
       END-IF                                                                 
                                                                              
           .                                                                  

       END PROGRAM Functions.
