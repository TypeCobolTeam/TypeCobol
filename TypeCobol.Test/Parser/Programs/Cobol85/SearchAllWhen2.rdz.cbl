       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 NotATable PIC X(20).
       
       01 TableWithoutKey.
          05 tab1 OCCURS 20.
             10 tab1-item PIC X.
       
       01 TableWithoutIndex.
          05 tab2 OCCURS 20
                  ASCENDING KEY IS tab2-key.
             10 tab2-key  PIC 99.
             10 tab2-item PIC XX.
       
       01 MultiDim1.
          05 tab3 OCCURS 10.
             10 tab3-1 OCCURS 10
                       ASCENDING KEY IS tab3-1-key
                       INDEXED BY tab3-1-idx.
                15 tab3-1-key  PIC 99.
                15 tab3-1-item PIC XX.
       
       01 test-value PIC XX.
       01 some-value PIC XX.
       
       01 MultiDim2.
          05 tab4 OCCURS 10
                  INDEXED BY tab4-idx1 tab4-idx2 tab4-idx3.
             10 tab4-1 OCCURS 10
                       INDEXED BY tab4-1-idx1 tab4-1-idx2 tab4-1-idx3.
                15 tab4-2 OCCURS 10
                          ASCENDING  KEY IS tab4-2-key-asc
                          DESCENDING KEY IS tab4-2-key-desc
                          INDEXED BY tab4-2-idx1 tab4-2-idx2
                                     tab4-2-idx3.
                   20 tab4-2-key-asc  PIC 99.
                   20 tab4-2-key-desc PIC 99.
                   20 tab4-2-item     PIC XX.
       
       01 some-key-value-1 PIC 99.
       01 some-key-value-2 PIC 99.
       
       01 MultiDim3.
          05 tab5 OCCURS 10.
             10 tab5-1 OCCURS 10.
                15 tab5-2 OCCURS 10
                          ASCENDING KEY IS tab5-2-key.
                   20 tab5-2-key  PIC 99.
                   20 tab5-2-item PIC XX.
       
       PROCEDURE DIVISION.
      *KO search on something that is not a table
           SEARCH ALL NotATable
              WHEN test-value = some-value
                 DISPLAY 'Ok'
           END-SEARCH
      *KO binary search on a table without key
           SEARCH ALL tab1
              WHEN test-value = some-value
                 DISPLAY 'Ok'
           END-SEARCH
      *KO binary search on a table without index
           SEARCH ALL tab2
              WHEN test-value = some-value
                 DISPLAY 'Ok'
           END-SEARCH
      *KO binary search on a table with parent table without index
           SEARCH ALL tab3-1
              WHEN test-value = some-value
                 DISPLAY 'Ok'
           END-SEARCH
      *KO all keys defined before any referenced key must be used
           SEARCH ALL tab4-2
              WHEN tab4-2-key-desc (tab4-idx1 tab4-1-idx1 tab4-2-idx1)
                   = some-key-value-1
                 DISPLAY 'Ok'
           END-SEARCH
      *KO must use equality
           SEARCH ALL tab4-2
              WHEN tab4-2-key-asc (tab4-idx1 tab4-1-idx1 tab4-2-idx1)
                   > some-key-value-1
                 DISPLAY 'Ok'
           END-SEARCH
      *KO must use AND
           SEARCH ALL tab4-2
              WHEN tab4-2-key-asc (tab4-idx1 tab4-1-idx1 tab4-2-idx1)
                   = some-key-value-1
                   OR
                   tab4-2-key-desc (tab4-idx1 tab4-1-idx1 tab4-2-idx1)
                   = some-key-value-2
                 DISPLAY 'Ok'
           END-SEARCH
      *KO not a table item comparison
           SEARCH ALL tab4-2
              WHEN tab4-2-key-asc (tab4-idx1 tab4-1-idx1 tab4-2-idx1)
                   IS NUMERIC
                 DISPLAY 'Ok'
           END-SEARCH
      *KO must use first index
           SEARCH ALL tab4-2
              WHEN tab4-2-key-asc (tab4-idx1 tab4-1-idx2 tab4-2-idx3)
                   = some-key-value-1
                 DISPLAY 'Ok'
           END-SEARCH
      *KO must use proper keys
           SEARCH ALL tab4-2
              WHEN tab5-2-key (tab4-idx1 tab4-1-idx1 tab4-2-idx1)
                   = some-key-value-1
                 DISPLAY 'Ok'
           END-SEARCH
      *KO key must be on left side
           SEARCH ALL tab4-2
              WHEN some-key-value-1
                   = tab4-2-key-asc (tab4-idx1 tab4-1-idx1 tab4-2-idx1)
                 DISPLAY 'Ok'
           END-SEARCH
      *OK
           SEARCH ALL tab4-2
              WHEN tab4-2-key-asc (tab4-idx1 tab4-1-idx1 tab4-2-idx1)
                   = some-key-value-1
                 DISPLAY 'Ok'
           END-SEARCH
           GOBACK
           .      
       END PROGRAM Pgm.