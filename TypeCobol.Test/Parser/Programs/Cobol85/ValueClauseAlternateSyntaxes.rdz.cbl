       identification division.
       program-id. DVZZMFT3.
       data division.
       working-storage section.
       01 var1 PIC X.
      *These two are standard
          88 case1 VALUE IS 'A'.
          88 case2 VALUES ARE 'B' 'C'.
      *These variants are not compliant with language specs
      *But still are accepted by IBM compiler.
          88 case3 VALUE ARE 'D' 'E'.
          88 case4 VALUES IS 'F' 'G'.
          88 case5 VALUE ARE 'H'.
          88 case6 VALUES IS 'I'.
       procedure division.
           goback
           .
       end program DVZZMFT3.