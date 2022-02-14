       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok
       01 MYCOPY.     COPY YMYCOPY.
      *Ok suffixing YMYCOPY with '1'
      *All string matching 'MYCOPY' will be suffixed by '1' (giving MYCOPY1)
       01 MYCOPY1.    COPY YMYCOPY1.
       PROCEDURE DIVISION.
      *   Ok
           Move 'A'             to MYCOPY-Data
      *   Ok because a version of YMYCOPY is also declared suffixed by '1'
           Move 'A'             to MYCOPY1-Data
           .
       END PROGRAM Pgm.