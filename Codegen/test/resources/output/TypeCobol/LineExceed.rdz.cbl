       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       DATA DIVISION .
       working-storage section.
       01  var1 pic X.
           88 value1 value 'A'.
      
       procedure division.
      *                                       SET var1::value1 TO true.
                                              SET value1 OF var1
                                                   TO true.
           .
       END PROGRAM MyPgm.
      
