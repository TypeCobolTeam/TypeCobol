       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       PROCEDURE DIVISION.
      
       replace ==:test1:== BY
      d        ==DISPLAY "test1"==.
           :test1:
      
      dreplace ==:test2:== BY
               ==DISPLAY "test2"==.
           :test2:
      
      dreplace ==:test3:== BY
      d        ==DISPLAY "test3"==.
           :test3:
      
       replace ==:test4a:== BY
      d        ==DISPLAY "test4a"==
               ==:test4b:== BY
      d        ==DISPLAY "test4b"==.
           :test4a:
           :test4b:
      
      dreplace ==:test5a:== BY
               ==DISPLAY "test5a"==
      d        ==:test5b:== BY
               ==DISPLAY "test5b"==.
           :test5a:
           :test5b:
      
           GOBACK
           .
       END PROGRAM TCOMFL06.