       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok because level 01 is not deleted for CPX/CPM
       COPY YTSTCPX.
       PROCEDURE DIVISION.
      *   Ok
           Move 'A'             to TSTCPX-Data
           .
       END PROGRAM Pgm.