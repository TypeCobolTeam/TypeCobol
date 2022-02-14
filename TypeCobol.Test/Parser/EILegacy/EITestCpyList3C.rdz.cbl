       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok for syntax but the REPLACING clause doesn't work without placeholder
       COPY YTSTCPX REPLACING ==TSTCPX== BY ==TSTCPX1==.
       PROCEDURE DIVISION.
      *   Ko because TSTCPX1-Data not declared
           Move 'A'             to TSTCPX1-Data
           .
       END PROGRAM Pgm.