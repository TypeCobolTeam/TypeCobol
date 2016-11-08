       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProcedureCall.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.

      *01  somedate     TYPE Date.                                            
       01 somedate.                                                           
           02 YYYY PIC 9(4).                                                  
           02 MM PIC 9(2).                                                    
           02 DD PIC 9(2).                                                    
       01  someformat   PIC X(08).
      *01  flag         TYPE Bool.                                            
       01  flag-value PIC X VALUE LOW-VALUE.                                  
           88  flag       VALUE 'T'.                                          
           88  flag-false VALUE 'F'.                                          
       01  realformat   PIC X(08).

       PROCEDURE DIVISION.
       
      *DECLARE FUNCTION ValidateDateFormat PRIVATE                            
      *    INPUT mydate        TYPE Date                                      
      *          format        PIC X(08)                                      
      *   OUTPUT okay          TYPE Bool                                      
      *          actual-format PIC X(08).                                     
      *  .                                                                    

       TRAITEMENT.
      *    CALL ValidateDateFormat                                            
      *             INPUT      somedate someformat                            
      *             OUTPUT     flag     realformat                            
       CALL ValidateDateFormat                                                
           USING  somedate someformat flag realformat                         
           .

       END PROGRAM ProcedureCall.
      *_________________________________________________________________      
       IDENTIFICATION DIVISION.                                               
       PROGRAM-ID. ValidateDateFormat-01.                                     
       DATA DIVISION.                                                         
       LINKAGE SECTION.                                                       
       01 mydate.                                                             
           02 YYYY PIC 9(4).                                                  
           02 MM PIC 9(2).                                                    
           02 DD PIC 9(2).                                                    
       01 format PIC X(08).                                                   
       01 okay-value PIC X     VALUE LOW-VALUE.                               
           88 okay       VALUE 'T'.                                           
           88 okay-false VALUE 'F'.                                           
       01 actual-format PIC X(08).                                            
       PROCEDURE DIVISION                                                     
             USING BY REFERENCE mydate                                        
                   BY REFERENCE format                                        
                   BY REFERENCE okay                                          
                   BY REFERENCE actual-format                                 
           .                                                                  
           CONTINUE.
       END PROGRAM ValidateDateFormat-01.                                     
