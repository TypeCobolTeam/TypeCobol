Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       DATA DIVISION .
       working-storage section.
       01  v pic X.
           88 value1 value 'A'.
      
       procedure division.
      *                                                      SET    v::
      *              value1 TO true.
                                                             SET    
                                                              value1 OF 
                     v TO true.
           .
       END PROGRAM MyPgm.
      
