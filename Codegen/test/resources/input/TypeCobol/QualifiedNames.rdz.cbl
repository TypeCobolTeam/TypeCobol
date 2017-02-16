﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID.   Test-Name-Qualification.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 a PIC 9.
       01 b PIC 9.
       
       01 Vector TYPEDEF.
           05 x PIC 9.
           05 y PIC 9.
           05 z PIC 9.
       
       01 Point TYPEDEF.
           02 Location TYPE Vector.
           02 Movment  TYPE Vector.
             04 Speed        TYPE Vector.
             04 Acceleration TYPE Vector.
       
       01 Segment.
           05 p TYPE POINT.
           05 q TYPE POINT.
       
       PROCEDURE DIVISION.
       
           MOVE a TO b
       
           MOVE a TO    x OF Location OF p
           MOVE a TO      p::Location   ::x
           MOVE   p :: Location :: y     TO   z OF   Location   IN p
           MOVE   1   TO x OF Location OF p
           MOVE 1 TO p :: Location ::    y z OF Location IN p

           MOVE p :: Location :: y TO p :: location :: y
           move p::location::y to p::location::y

           if Segment::p = Segment::q
             continue
           end-if

           evaluate true
              when Segment::p::Location::x = 1
                  continue
              when Segment::p::Location::y = 1
                  continue
              when other
                  compute Segment::p::Location::y = 9
           end-evaluate
           .
       
       END PROGRAM Test-Name-Qualification.