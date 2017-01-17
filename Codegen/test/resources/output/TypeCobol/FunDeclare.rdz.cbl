       IDENTIFICATION DIVISION.
       PROGRAM-ID. FunDeclare.
       PROCEDURE DIVISION.

      *DECLARE FUNCTION DoesNothing PRIVATE.                                  

      *DECLARE FUNCTION ReturnsZero PRIVATE                                   
      *      RETURNING result PIC 9(04).                                      

      *OK: second function with same name, but profile is different
      *DECLARE FUNCTION DoesNothing PRIVATE                                   
      *      INPUT x PIC 9(04).                                               

      *DECLARE FUNCTION StrangelyReturnsItsInput PRIVATE                      
      *      INPUT     x      PIC 9(04)                                       
      *      RETURNING result PIC 9(04)                                       
      *    .                                                                  

      *written in lower-case to make sure code generation doesn't 
      *change it to upper-case
      *declare function UseACopy private                                      
      *         input  x pic X.                                               


       ILLEGAL-NON-FUNCTION-PARAGRAPH.
           CONTINUE.
       
       END PROGRAM FunDeclare.
      *                                                                       
      *DECLARE FUNCTION DoesNothing PRIVATE.                                  
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. a59b2c49.                                                  
       PROCEDURE DIVISION                                                     
           .                                                                  
           DISPLAY 'I DO NOTHING'
           .
       END PROGRAM a59b2c49.                                                  
      *                                                                       
      *DECLARE FUNCTION ReturnsZero PRIVATE                                   
      *             RETURNING result PIC 9(04).                               
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
      *                                                                       
      *DECLARE FUNCTION DoesNothing PRIVATE                                   
      *             INPUT x PIC 9(04).                                        
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
      *                                                                       
      *DECLARE FUNCTION StrangelyReturnsItsInput PRIVATE                      
      *             INPUT     x      PIC 9(04)                                
      *             RETURNING result PIC 9(04)                                
      *           .                                                           
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. e3e490ae.                                                  
       DATA DIVISION.
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
      *                                                                       
      *declare function UseACopy private                                      
      *                input  x pic X.                                        
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. f6a89a72.                                                  
       data division.
       working-storage section.
       01 yoto pic X.
       REPLACE ==:MyPrefix:== by ==MyPrefix2==.
       COPY MyDataCopy.
       LINKAGE SECTION.                                                       
       01 x PIC X.                                                            
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE x                                             
           .                                                                  
           display "Hello"
           COPY MyProcedureCopy.
           .
       END PROGRAM f6a89a72.                                                  
