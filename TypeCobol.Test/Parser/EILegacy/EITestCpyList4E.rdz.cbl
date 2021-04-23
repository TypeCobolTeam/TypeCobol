       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ko because YTSTCPM1 doesn't exist
       COPY YTSTCPM1 REPLACING ==:TSTCPM:== BY ==:TSTCPM1:==.
       PROCEDURE DIVISION.
      *   Ko because TSTCPM1-Data si not declared
           Move 'A'             to TSTCPM1-Data
           .
       END PROGRAM Pgm.