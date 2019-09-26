       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
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
       END PROGRAM Pgm.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
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
      