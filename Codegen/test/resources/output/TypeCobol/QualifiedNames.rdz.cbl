       IDENTIFICATION DIVISION.
       PROGRAM-ID.   Test-Name-Qualification.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 a PIC 9.
       01 b PIC 9.
       
      *01 Vector TYPEDEF.                                                     
      *    05 x PIC 9.                                                        
      *    05 y PIC 9.                                                        
      *    05 z PIC 9.                                                        
       
      *01 Point TYPEDEF.                                                      
      *    02 Location TYPE Vector.                                           
      *    02 Movment  TYPE Vector.                                           
      *      04 Speed        TYPE Vector.                                     
      *      04 Acceleration TYPE Vector.                                     
       
       01 Segment.
      *    05 p TYPE POINT.                                                   
           05 p.
           06 Location.
             07 x PIC 9.
             07 y PIC 9.
             07 z PIC 9.
           06 Movment.
             07 x PIC 9.
             07 y PIC 9.
             07 z PIC 9.
                           
      *    05 q TYPE POINT.                                                   
           05 q.
           06 Location.
             07 x PIC 9.
             07 y PIC 9.
             07 z PIC 9.
           06 Movment.
             07 x PIC 9.
             07 y PIC 9.
             07 z PIC 9.
                           
       
       PROCEDURE DIVISION.
       
           MOVE a TO b
       
           MOVE a TO    x OF Location OF p
      *    MOVE a TO      p::Location   ::x                                   
           MOVE a TO      x OF Location    OF p
      *    MOVE   p :: Location :: y     TO   z OF   Location   IN p          
           MOVE   y  OF  Location  OF  p     TO   z OF   Location   IN p
           MOVE   1   TO x OF Location OF p
      *    MOVE 1 TO p :: Location ::    y z OF Location IN p                 
           MOVE 1 TO y  OF  Location  OF     p z OF Location IN p

      *    MOVE p :: Location :: y TO p :: location :: y                      
           MOVE y  OF  Location  OF  p TO y  OF  location  OF  p
      *    move p::location::y to p::location::y                              
           move y OF location OF p to y OF location OF p
           .
       
       END PROGRAM Test-Name-Qualification.
