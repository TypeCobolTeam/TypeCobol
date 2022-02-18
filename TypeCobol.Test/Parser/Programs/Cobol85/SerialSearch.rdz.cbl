       IDENTIFICATION DIVISION.
       PROGRAM-ID. SerialSearch.
       data division.
       working-storage section.
       01 tab.
          05 var1 occurs 10 ascending key is Var2 indexed by idx .
            10 var2 pic X(1) .
      
       PROCEDURE DIVISION.
      
           search var1
              when var2(idx) = 'a'
                 display "search"
              when var2(idx) = 'b'
                 display "search"
           end-search
      
      
           search var1
      *KO when without instruction
              when var2(idx) = 'a'
              when var2(idx) = 'b'
                 display "search"
           end-search
      
      
           search var1
      *KO when without instruction
              when var2(idx) = 'b'
           end-search
      
           goback.
       END PROGRAM SerialSearch.