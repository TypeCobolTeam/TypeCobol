       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Group1.
           05 item-1 PIC X
      *KO value start in col 11
           VALUE 'A'.
           05 item-2 PIC X
      *OK
          VALUE '2'.
      
       PROCEDURE DIVISION.
      
           GOBACK
           .
       END PROGRAM DVZF0OSM.