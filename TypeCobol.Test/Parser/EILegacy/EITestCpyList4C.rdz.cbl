       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ko because YTSTCPM1 doesn't exist
       COPY YTSTCPM1 REPLACING ==:TSTCPM:== BY ==:TSTCPM:==.
       PROCEDURE DIVISION.
           .
       END PROGRAM Pgm.