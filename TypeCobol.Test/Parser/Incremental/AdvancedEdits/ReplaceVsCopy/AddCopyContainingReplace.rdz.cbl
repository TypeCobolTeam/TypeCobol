       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       REPLACE ==:suffix:== BY ==suffixFromMainPgm==.
      *First incremental change: add a COPY containing REPLACE here
      *Second incremental change: add a space on copy line to trigger
      *proper reparsing
       01 var1 PIC X.
       01 var2 PIC X.
       01 var-other-:suffix: PIC X.
       END PROGRAM DVZF0OSM.