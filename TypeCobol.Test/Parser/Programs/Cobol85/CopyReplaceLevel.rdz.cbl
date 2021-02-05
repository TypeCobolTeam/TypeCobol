       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MyGroup.
           COPY YXXXTEN REPLACING  ==:XXXTEN:==  BY ==XXXTEN==
                                   ==01 :XXXTEN:== BY ==02 XXXTEN==.
      
       END PROGRAM DVZF0OSM.