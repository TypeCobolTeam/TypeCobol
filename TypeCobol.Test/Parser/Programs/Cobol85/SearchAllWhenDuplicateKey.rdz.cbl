       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Pgm.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01.
        02 DSRCE-TAB  OCCURS 83
                      ASCENDING KEY IS  DSRCE-KEY-A
      * KO, duplicate key               
                      DESCENDING KEY IS DSRCE-KEY-A
                      INDEXED BY Idx, Idx2.
            05 DSRCE-KEY-A           Pic 99.
            05 DSRCE-KEY-D           Pic 99.
            05 DSRCE-SRCE-VAL        Pic S9(3).
      
       01 WS-KEY                     Pic 99.
       01 WS-KEY2                    Pic 99.
       01 WS-SRCE-VAL                Pic S9(3).
       01 I                          Pic 99.
      
       PROCEDURE DIVISION.
      
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ok
              WHEN DSRCE-KEY-A (Idx) = WS-KEY
                 DISPLAY 'VALUE FOUND'
           END-SEARCH

           GOBACK
           .
      
       END PROGRAM Pgm.