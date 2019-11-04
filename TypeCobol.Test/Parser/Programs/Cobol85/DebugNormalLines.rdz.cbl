       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370
           WITH DEBUGGING MODE.
       DATA DIVISION.
       Working-Storage Section.
       01 xxxx pic XXXX value 'toto'.
       01 yyyy pic XXXX.
       01 x pic 999 value 0.
       PROCEDURE DIVISION.
      
      d    if xxxx = 'toto'
      d         display "hhh"
           end-if
      
           if xxxx = 'toto'
      d         display "hhh"
           end-if
      
      d    if xxxx = 'toto'
      d         display "hhh"
                 perform 5 times
      d            compute x = x + 5
                 end-perform
      d    end-if
      
           if xxxx = 'toto'
              display xxxx
      D    else
              move xxxx to yyyy
           end-if

      d    evaluate xxxx
      d         when 'toto' display "found"
                when other display "not found"
           end-evaluate
   
           GOBACK.
       END PROGRAM Pgm.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       Working-Storage Section.
       01 xxxx pic XXXX value 'toto'.
       PROCEDURE DIVISION.
      d    if xxxx = 'toto'
                display "hi"
           end-if
           GOBACK.
       END PROGRAM Pgm2.
      