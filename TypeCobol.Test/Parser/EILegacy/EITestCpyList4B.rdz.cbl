       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok
       COPY YTSTCPM REPLACING ==:TSTCPM:== BY ==TSTCPM==.
       PROCEDURE DIVISION.
      *   Ok
           Move 'A'             to TSTCPM-Data
           .
       END PROGRAM Pgm.