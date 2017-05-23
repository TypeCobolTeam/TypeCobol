       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       DATA DIVISION .
       working-storage section.
       01  var1 pic X.
           88 value1 value 'A'.
      
       procedure division.
      *                                       SET var1::value1 TO true.
                                              SET value1 OF var1 TO true.
           .
       END PROGRAM MyPgm.
      
      * 1 errors
      * Line 9[0,0] <38, Error, Semantics> - Error during Cobol generation: generated line is after column 72 in fixed format or line exceed 80 columns
