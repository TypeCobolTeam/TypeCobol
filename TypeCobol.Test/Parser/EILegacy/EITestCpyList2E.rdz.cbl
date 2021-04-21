       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok for declaration but suffixing YMYCOPY not working with placeholder, 
      *the REPLACING will suffix MYCOPY1
       01 MYCOPY1. COPY YMYCOPY1 REPLACING ==:MYCOPY:== BY ==MYCOPY1==.
       PROCEDURE DIVISION.
      *   Ok thanks to REPLACING clause
           Move 'A'             to MYCOPY1-Data
           .
       END PROGRAM Pgm.