       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       data division.
       working-storage section.
      *Issue #2121, this would previously throw an InvalidCastException
       01 group1.
          EXEC whatever
             this exec statement is not correctly ended.
       procedure division.
           goback
           .
       END PROGRAM TCOMFL06.