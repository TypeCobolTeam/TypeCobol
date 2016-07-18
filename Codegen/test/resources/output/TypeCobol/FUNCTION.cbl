      * 9 CodeElements errors
      * "1"@(30:12>30:42): [27:1] Syntax error : Function POW is missing parameter 1 of type Numeric
      * "1"@(30:12>30:42): [27:1] Syntax error : Function POW is missing parameter 2 of type Numeric
      * "1"@(31:12>31:42): [27:1] Syntax error : Function POW is missing parameter 2 of type Numeric
      * "1"@(32:12>32:42): [27:1] Syntax error : Function POW only takes 2 parameters
      * "1"@(34:12>34:42): [27:1] Syntax error : Symbol i is not referenced
      * "1"@(34:12>34:42): [27:1] Syntax error : Symbol j is not referenced
      * "1"@(36:12>36:42): [27:1] Syntax error : Function POW expected parameter 2 of type Numeric (actual: BOOL)
      * "1"@(38:12>38:42): [27:1] Syntax error : Function POW expected parameter 2 of max length 3 (actual: 5)
      * "1"@(40:12>40:42): [27:1] Syntax error : Symbol POWAAA is not referenced
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Functions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  x PIC 9.
       01  y PIC 9(3).
       01  z PIC 9(5).
       01  b TYPE BOOL.
       01 Functionscpy COPY Functionscpy.                                     
       01 Functions PIC X(08) VALUE 'Functions'.                              
                                                                              
       01 RETURN-CODE PIC X(08).                                              
                                                                              
       01 POW-RESULT PIC 9(8).                                                
                                                                              

       PROCEDURE DIVISION.
       IF Functionscpy-POINTER-TABLE = LOW_VALUE                              
           CALL Functions USING Functionscpy                                  
       END-IF                                                                 
                                                                              
       
      *DECLARE function POW PRIVATE.                                          
       PROGRAM-ID. POW.                                                       
         DATA DIVISION.
         LINKAGE SECTION.
           01 x PIC 9(05).
           01 y PIC 9(03).
           01 result PIC 9(08).
         PROCEDURE DIVISION                                                   
             USING BY REFERENCE x                                             
                   BY REFERENCE y                                             
             RETURNING result                                                 
         .                                                                    
           CONTINUE.
       END PROGRAM POW.                                                       

       TRAITEMENT.
      *    MOVE FUNCTION POW (x y)    TO x                                    
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
                                                                              
      * KO: wrong number of parameters
      *    MOVE FUNCTION POW ()       TO x                                    
       CALL POW USING                                                         
           BY CONTENT SPACE                                                   
           BY CONTENT SPACE                                                   
                                                                              
           BY REFERENCE RETURN-CODE                                           
           BY REFERENCE POW-RESULT                                            
                                                                              
       IF RETURN-CODE = ZERO                                                  
           MOVE POW-RESULT TO x                                               
       ELSE                                                                   
      *    TODO: error management                                             
       END-IF                                                                 
                                                                              
      *    MOVE FUNCTION POW (y)      TO x                                    
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
                                                                              
      *    MOVE FUNCTION POW (x y z)  TO x                                    
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
                                                                              
      * KO: undefined parameters
      *    MOVE FUNCTION POW (i j)    TO x                                    
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
                                                                              
      * KO: 2nd parameter of wrong type
      *    MOVE FUNCTION POW (x b)    TO x                                    
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
                                                                              
      * KO: 2nd parameter too-large
      *    MOVE FUNCTION POW (z z)    TO x                                    
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
                                                                              
      * KO: function undeclared
           MOVE FUNCTION POWAAA (x y) TO x
           .

       END PROGRAM Functions.
