Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
      *
      *REMARKS. COPY=(
      *        MyCOPY2
      *        YCCTEXT
      *        YCCTEX2
      *        YCCTEX3
      *        ).
                 
       DATA DIVISION.
       working-storage section.
       01 CCTEXT. COPY YCCTEXT.
       PROCEDURE DIVISION.
           continue
           .
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       DATA DIVISION.
       working-storage section.
       01 CCTEX2. COPY YCCTEX2.
       END PROGRAM PGM2.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM3.
       DATA DIVISION.
       working-storage section.
       01 CCTEX3. COPY YCCTEX3.
       END PROGRAM PGM3.
       END PROGRAM DVZF0OSM.
