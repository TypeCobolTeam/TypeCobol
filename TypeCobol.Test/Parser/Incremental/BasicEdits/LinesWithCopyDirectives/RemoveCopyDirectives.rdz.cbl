       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *CPX on its own line
       COPY YCPXAAA.
       COPY YCPXAAB.
       COPY YCPXAAC.
       
      *CPX mixed on a single line
       COPY YCPXABA. COPY YCPXABB. COPY YCPXABC.
       
      *CPY without suffix on its own line
       01 a. COPY YCPYAAA.
       01 b. COPY YCPYAAB.
       01 c. COPY YCPYAAC.
       
      *CPY without suffix mixed on a single line
       01 a. COPY YCPYABA. 01 b. COPY YCPYABB. 01 c. COPY YCPYABC.

      *CPY with suffix on its own line
       01 a1. COPY YCPYAAA1.
       01 a2. COPY YCPYAAA2.
       01 a3. COPY YCPYAAA3.
       
      *CPY with suffix mixed on a single line
       01 a. COPY YCPYABA1. 01 b. COPY YCPYABA2. 01 c. COPY YCPYABA3.
       
       END PROGRAM TCOMFL06.