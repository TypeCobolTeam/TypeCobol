       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok for syntax but the REPLACING clause doesn't work without placeholder
       01 MYCOPY1.    COPY YMYCOPY REPLACING ==MYCOPY== BY ==MYCOPY1==.
       PROCEDURE DIVISION.
      *   Ko MYCOPY1-Data is not defined 
      *   (attempted suffixing by '1' failed because of placeholder)
           Move 'A'             to MYCOPY1-Data
           .
       END PROGRAM Pgm.