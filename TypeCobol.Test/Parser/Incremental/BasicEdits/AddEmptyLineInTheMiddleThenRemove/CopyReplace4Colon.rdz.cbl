       IDENTIFICATION division.
       PROGRAM-ID. DVZS0OSM.
       data division.
       working-storage section.
       COPY YDVZOSM replacing ==::== by ====
                                         ==:DVZOSM:== by ==DVZOSM==.
       COPY YDVZOSM replacing ==::== by ==2==
                                         ==:DVZOSM:== by ==Foo==.
       procedure division.
           move 'A' to DVZOSM-A
           move 'A' to Foo-A2
           .
       end program DVZS0OSM.