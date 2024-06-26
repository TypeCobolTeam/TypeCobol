       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01.
           02 DSRCE-TAB  OCCURS 83
                             ASCENDING KEY IS  DSRCE-KEY-A
                                               DSRCE-KEY-B
                             INDEXED BY Idx.
             05 DSRCE-KEY-A           Pic X.
                88 DSRCE-KEY-A-val1 value "A".
             05 DSRCE-KEY-B  pic X.
      
       PROCEDURE DIVISION.
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ok, no error expected
              WHEN DSRCE-KEY-A(Idx) = "A"
                 and DSRCE-KEY-B(Idx) = "A"
                 DISPLAY 'VALUE FOUND'
           END-SEARCH

      * Same search but with level 88 DSRCE-KEY-A-val1
      * instead of DSRCE-KEY-A
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ok, no error expected
              WHEN DSRCE-KEY-A-val1 (Idx)
                 and DSRCE-KEY-B(Idx) = "A"
                 DISPLAY 'VALUE FOUND'
           END-SEARCH
           goback
           .
       END PROGRAM MyPGM.