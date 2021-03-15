       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       Working-Storage Section.
       01 xxxx pic XXXX value 'toto'.
       01 yyyy pic XXXX.
       PROCEDURE DIVISION.

      *Normal line then debug (ignored) then valid code
           MOVE xxxx TO yyyy
      d    DISPLAY 'name = '
                yyyy
      d    DISPLAY 'name = ' yyyy

      *Normal line then debug (ignored) then invalid code
           MOVE '1234' TO yyyy
      d    DISPLAY 
           'name = ' yyyy

      *Normal line ended with dot followed by mix debug
           MOVE xxxx TO yyyy.
      d    DISPLAY 'name = '
                yyyy
           GOBACK.
       END PROGRAM Pgm.
       