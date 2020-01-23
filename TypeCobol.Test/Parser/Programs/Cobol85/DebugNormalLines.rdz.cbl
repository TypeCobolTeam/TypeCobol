       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       DATA DIVISION.
       Working-Storage Section.
       01 xxxx pic XXXX value 'toto'.
       01 yyyy pic XXXX.
       01 x pic 999 value 0.
       PROCEDURE DIVISION.
           MOVE xxxx TO yyyy
      *Normal then debug line
      D    DISPLAY 'name = '
                yyyy
           MOVE '1234' TO yyyy
      
      *Debug line in the middle
           DISPLAY 'name = '
      d        yyyy
               '_1'
      
      *Normal line in the middle
      d    DISPLAY 'name = '
               yyyy
      d        '_2'
      
      *Empty line before debug line
           DISPLAY 'name = '
               yyyy
      
      d        '_3'
      
      *Empty line before normal line
      d    DISPLAY 'name = '
      
              yyyy
      
      *Empty line before normal line
      d    DISPLAY 'name = '
      d       yyyy
      
             '_4'
      
           GOBACK.
       END PROGRAM Pgm.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       Working-Storage Section.
       01 xxxx pic XXXX value 'toto'.
       01 yyyy pic XXXX.
       PROCEDURE DIVISION.
           MOVE xxxx TO yyyy
      d    DISPLAY 'name = '
                yyyy
           MOVE '1234' TO yyyy
           GOBACK.
       END PROGRAM Pgm2.
      
      