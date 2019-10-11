       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER1.
       procedure division.
       main section.
      
      *> And now do some altering.
       entrance.
           ALTER front TO PROCEED TO atstart
           GO TO front
           .
      
      *> Jump to a part of the front
       front.
           GO TO atstart.
      
      *> the first part
       atstart.
           ALTER front TO PROCEED to middle
           DISPLAY "This is the start of a changing front"
           GO TO front
           .
      
      *> the middle bit
       middle.
           ALTER front TO PROCEED to atfinish
           DISPLAY "The front progresses"
           GO TO front
           .
      
      *> the climatic finish
       atfinish.
           DISPLAY "The front ends, happily ever after"
           .
      
      *> fall through to the exit
       END PROGRAM ALTER1.
      