       IDENTIFICATION DIVISION.
       PROGRAM-ID. SearchAll.
       data division.
       working-storage section.
       01 tab.
          05 var1 occurs 10 ascending key is Var2 indexed by idx .
            10 var2 pic X(1) .
      
       PROCEDURE DIVISION.    
      
      *-------- ONLY WHEN
      *OK, when with instruction
           search all var1
              when var2(idx) = 'b'
                display "test"
           end-search
      
      *OK, when without instruction
           search all var1
              when var2(idx) = 'b'
      
           end-search
      
      *-------- AT END and WHEN
      *OK
           search all var1
              at end
                display "end"
              when var2(idx) = 'b'
                display "when"
           end-search
      
      *OK
           search all var1
              at end
                display "end"
              when var2(idx) = 'b'
           end-search
      
      
      *-------- AT END at wrong position
           search all var1
              when var2(idx) = 'b'
                display "when"
      *KO at end must be before when statement
              at end
                display "end"
           end-search
      
           search all var1
              when var2(idx) = 'b'
      *KO at end must be before when statement
              at end
                display "end"
           end-search

      *-------- 2 WHEN always KO
           search all var1
              when var2(idx) = 'a'
                display "when a"
              when var2(idx) = 'b'
                display "when b"
           end-search
      
           search all var1
              at end
                display "end"
              when var2(idx) = 'a'
                display "when a"
              when var2(idx) = 'b'
                display "when b"
           end-search
      
           search all var1
              at end
                display "end"
              when var2(idx) = 'a'
              when var2(idx) = 'b'
                display "when a and b"
           end-search
           goback.
       END PROGRAM SearchAll.