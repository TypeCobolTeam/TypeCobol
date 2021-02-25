       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Pgm.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01.
        02 DSRCE-TAB  OCCURS 83
                      ASCENDING KEY IS  DSRCE-KEY-A
                      DESCENDING KEY IS DSRCE-KEY-D
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

           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ok
              WHEN WS-KEY = DSRCE-KEY-A (Idx)
                 DISPLAY 'VALUE FOUND'
           END-SEARCH
      
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ko DSRCE-SRCE-VAL is not a key of the table searched
              WHEN DSRCE-SRCE-VAL (Idx) = WS-KEY
                 DISPLAY 'VALUE FOUND'
           END-SEARCH

           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ko DSRCE-SRCE-VAL is not a key of the table searched
              WHEN WS-KEY = DSRCE-SRCE-VAL (Idx)
                 DISPLAY 'VALUE FOUND'
           END-SEARCH
      
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ok
              WHEN DSRCE-KEY-A (Idx) = WS-KEY
               AND DSRCE-KEY-D (Idx) = WS-KEY2
                 DISPLAY 'VALUE FOUND'
           END-SEARCH
      
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
              WHEN DSRCE-KEY-A (Idx) = WS-KEY
      *  Ko DSRCE-SRCE-VAL is not a key of the table searched
               AND DSRCE-SRCE-VAL (Idx) = WS-KEY2
                 DISPLAY 'VALUE FOUND'
           END-SEARCH
      
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ko DSRCE-KEY-A is not indexed by the first index of DSRCE-TAB
              WHEN DSRCE-KEY-A (Idx2) = WS-KEY
                 DISPLAY 'VALUE FOUND'
           END-SEARCH
      
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
      *  Ko DSRCE-KEY-A is not indexed by the first index of DSRCE-TAB
              WHEN DSRCE-KEY-A (I) = WS-SRCE-VAL
                 DISPLAY 'VALUE FOUND'
           END-SEARCH
      
           SEARCH ALL DSRCE-TAB
              AT END
                 DISPLAY 'VALUE NOT FOUND'
              WHEN DSRCE-KEY-A (Idx) = WS-KEY
      *  Ko DSRCE-KEY-D is not indexed by the first index of DSRCE-TAB
               AND DSRCE-KEY-D (Idx2) = WS-KEY2
                 DISPLAY 'VALUE FOUND'
           END-SEARCH
      
           GOBACK
           .
      
       END PROGRAM Pgm.