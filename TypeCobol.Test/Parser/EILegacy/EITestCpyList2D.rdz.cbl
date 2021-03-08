       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok for declaration but suffixing YMECOPY not working with placeholder
       01 MECOPY1. COPY YMECOPY1 REPLACING ==:MECOPY:== BY ==MECOPY==.
       PROCEDURE DIVISION.
      *   Ko special CPY suffixing mechanism doesn't apply with placeholder
           Move 'A'             to MECOPY1-Data
      *Ok special CPY suffixing mechanism doesn't apply with placeholder
           Move 'A'             to MECOPY-Data
           .
       END PROGRAM Pgm.