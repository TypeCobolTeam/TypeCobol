       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZS0OSM.
      
       data division.
       working-storage section.
      
       01 MyVar pic X.
          88 MyVar-A value "A".
          88 MyVar-B value "B".
      
       procedure division.
      *OK
           if MyVar-A
                    continue
           else
                    continue
           end-if
      
      *KO, end-if missing
           if MyVar = "A"
                continue
           else
              if MyVar = "B"
                continue
              end-if
      
      *KO, end-if missing
           if MyVar-A
                continue
      
      
      *KO, end-if missing
           if MyVar = "A"
                continue
           else
      *    KO, end-if missing
              if MyVar = "B"
                continue
      
      *OK
           if MyVar = "A"
                continue
           else
      *       OK
              if MyVar = "B"
                continue
              end-if
           end-if
           .
       END PROGRAM DVZS0OSM.