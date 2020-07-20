       IDENTIFICATION DIVISION.
       PROGRAM-ID.   Test-Name-Qualification.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 a PIC 9.
       01 b PIC 9.
       
      *01 Vector TYPEDEF strict.
      *    05 x PIC 9.
      *    05 y PIC 9.
      *        88 toto value 1.
      *    05 z PIC 9.
       
      *01 Point TYPEDEF strict.
      *    02 Location TYPE Vector.
      *    02 Movment.
      *      04 Speed        TYPE Vector.
      *      04 Acceleration TYPE Vector.
       
       01 MySegment.
      *    05 p TYPE POINT.
           05 p.
           06 Location.
             07 x PIC 9.
             07 y PIC 9.
                88 toto value 1.
             07 z PIC 9.
           06 Movment.
             07 Speed.
               08 x PIC 9.
               08 y PIC 9.
                  88 toto value 1.
               08 z PIC 9.
             07 Acceleration.
               08 x PIC 9.
               08 y PIC 9.
                  88 toto value 1.
               08 z PIC 9.
                           
      *    05 q TYPE POINT.
           05 q.
           06 Location.
             07 x PIC 9.
             07 y PIC 9.
                88 toto value 1.
             07 z PIC 9.
           06 Movment.
             07 Speed.
               08 x PIC 9.
               08 y PIC 9.
                  88 toto value 1.
               08 z PIC 9.
             07 Acceleration.
               08 x PIC 9.
               08 y PIC 9.
                  88 toto value 1.
               08 z PIC 9.
                           
       
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
      *    set MySegment::p::Location::y::Toto to true
           set Toto OF y OF Location OF p OF MySegment to true
      *    MOVE p :: Location :: y TO p :: location :: y
           MOVE y  OF  Location  OF  p TO y  OF  location  OF  p
      *    move p::location::y to p::location::y
           move y OF location OF p to y OF location OF p

      *    if MySegment::p = MySegment::q
           if p OF MySegment = q OF MySegment
             continue
           end-if

           evaluate true
      *       when MySegment::p::Location::x = 1
              when x OF Location OF p OF MySegment = 1
                  continue
      *       when MySegment::p::Location::y = 1
              when y OF Location OF p OF MySegment = 1
                  continue
              when other
      *           compute MySegment::p::Location::y = 9
                  compute y OF Location OF p OF MySegment = 9
           end-evaluate
           .
       
       END PROGRAM Test-Name-Qualification.
