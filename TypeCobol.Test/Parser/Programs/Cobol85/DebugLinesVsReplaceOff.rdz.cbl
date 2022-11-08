       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM1.
       REPLACE OFF.
       PROCEDURE DIVISION.
      *Ok debugging mode is off
      D    MOVE a TO b
           GOBACK
           .
       END PROGRAM MyPGM1.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       REPLACE OFF.
       PROCEDURE DIVISION.
      *Ko debugging mode is active and a and b are not defined
      D    MOVE a TO b
           GOBACK
           .
       END PROGRAM MyPGM2.