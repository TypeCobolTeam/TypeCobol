       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MyGroup.
           COPY YXXXENT REPLACING  ==:XXXENT:==  BY ==XXXENT==
                                   ==01 :XXXENT:== BY ==02 XXXENT==.
      
       END PROGRAM DVZF0OSM.
