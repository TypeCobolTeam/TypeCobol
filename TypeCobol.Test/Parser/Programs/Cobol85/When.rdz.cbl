       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       data division.
       working-storage section.
       01 var1 pic X(5) value 'hi'.
       01 var2 pic X(5) value 'ho'.
       01 var3 pic X(5) value 'ho'.
       01 bool1 type bool.
       01 bool2 type bool.
       PROCEDURE DIVISION.
           evaluate true
                when
                when bool1 display "evaluate"
                when bool1
                     when bool2 display "evaluate"
                when
                when other continue
           end-evaluate
           evaluate true
                when bool1 display "evaluate"
                when bool1
                when other continue
           end-evaluate      
           search var1
                when
                when var2
                    when var3 display "search"
                when
           end-search
           evaluate true
                when bool1 display "evaluate"
      *KO missing statement in "when other" clause
                when other
           end-evaluate      
           move "hi" to var1
           goback.
       END PROGRAM Pgm.