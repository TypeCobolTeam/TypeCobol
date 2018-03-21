       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       DATA DIVISION .
       working-storage section.
       01  v pic X.
           88 value1 value 'A'.
      
       procedure division.
                                                             SET    v::
                     value1 TO true.
           .
       END PROGRAM MyPgm.
      