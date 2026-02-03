       IDENTIFICATION DIVISION.
       PROGRAM-ID. WhenTest.
       data division.
       working-storage section.
       01 var1 pic X(5) value 'hi'.
       01 var2 pic X(5) value 'ho'.
       01 var3 pic X(5) value 'ho'.
       01 bool1 pic 9.
       01 bool2 pic 9.
       PROCEDURE DIVISION.
           evaluate true
                when
                when bool1 display "evaluate"
                when bool1
                     when bool2 display "evaluate"
                when
                when other continue
           evaluate true
                when bool1 display "evaluate"
                when bool1
                when other continue
           search var1
                when
                when var2 = "ho"
                    when var3 = "ho" display "search"
                when
           end-search
           evaluate true
                when bool1 display "evaluate"
                when other
           move "hi" to var1
           goback.
       END PROGRAM WhenTest.