       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok
       01 MECOPY. COPY YMECOPY REPLACING ==:MECOPY:== BY ==MECOPY==.
      *Ok the placeholder can be replaced with an added suffix
       01 MECOPY1. COPY YMECOPY REPLACING ==:MECOPY:== BY ==MECOPY1==.
       PROCEDURE DIVISION.
      *   Ok
           Move 'A'             to MECOPY-Data
      *   Ok
           Move 'A'             to MECOPY1-Data
           .
       END PROGRAM Pgm.