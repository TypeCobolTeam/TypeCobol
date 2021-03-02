       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok for declaration but suffixing YMYCOPY not working with placeholder
       01 MYCOPY1. COPY YMYCOPY1 REPLACING ==:MYCOPY:== BY ==MYCOPY==.
       PROCEDURE DIVISION.
      *   Ko special CPY suffixing mechanism doesn't apply with placeholder
           Move 'A'             to MYCOPY1-Data
           .
       END PROGRAM Pgm.