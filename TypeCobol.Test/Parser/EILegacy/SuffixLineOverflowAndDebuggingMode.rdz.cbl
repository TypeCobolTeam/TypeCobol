       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       data division.
       working-storage section.
       01 CCCDDD. COPY YCCCDDDZ.

       procedure division.
      *    Ko CCCDDD is defined twice
           move "A" to CCCDDD
           .
       END PROGRAM MyPGM.