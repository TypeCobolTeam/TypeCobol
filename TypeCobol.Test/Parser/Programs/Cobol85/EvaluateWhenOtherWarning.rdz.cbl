       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZS0OSM.

       data division.
       working-storage section.

       01 MyVar pic X.
          88 MyVar-A value "A".
          88 MyVar-B value "B".

       procedure division.
           evaluate true
             when MyVar-A
                continue
             when MyVar-B
                continue
           end-evaluate
           

           evaluate MyVar
             when "A"
               continue
             when "B"
               continue
           end-evaluate
      
           evaluate true
             when MyVar-A
               continue
             when MyVar-B
               continue
             when other
               continue
           end-evaluate
           .
       END PROGRAM DVZS0OSM.