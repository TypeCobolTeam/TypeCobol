       IDENTIFICATION division.
       PROGRAM-ID. DVZS0OSM.
       data division.
       working-storage section.
       COPY YDVZOSM replacing ==::== by ====
                                         ==:MDVZOSM:== by ==MDVZOSM==.
       COPY YDVZOSM replacing ==::== by ==2==
                                         ==:MDVZOSM:== by ==Foo==.
       procedure division.
           move 'A' to MDVZOSM-A
           move 'A' to Foo-A2
           .
       end program DVZS0OSM.
