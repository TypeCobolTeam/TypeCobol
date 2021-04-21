       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok
       COPY YTSTCPM REPLACING ==:TSTCPM:== BY ==TSTCPM1==.
       PROCEDURE DIVISION.
      *   Ok
           Move 'A'             to TSTCPM1-Data
           .
       END PROGRAM Pgm.