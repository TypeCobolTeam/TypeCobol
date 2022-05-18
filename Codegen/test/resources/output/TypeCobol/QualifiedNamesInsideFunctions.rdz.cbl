       IDENTIFICATION DIVISION.
       PROGRAM-ID.   Test-Name-Qualification.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *01 _EVENT_ TYPEDEF STRICT.
      *    05 IdNbr PIC X(05).
      *    05 Periodicity PIC 9(03).
      
       01 EventList.
      *    05 Event TYPE _EVENT_ OCCURS 200.
           05 Event OCCURS 200.
           06 IdNbr PIC X(05).
           06 Periodicity PIC 9(03).
                                            
              
       01 LK-RETENTION PIC 9(4).
      
      *01 Person TYPEDEF STRICT.
      *    05 LastName pic X(30).
      *    05 FirstName pic X(30).
      *    05 BirthDate type date.
      
       01 Group1.
      *    05 AccountOwner TYPE Person.
           05 AccountOwner.
           06 LastName pic X(30).
           06 FirstName pic X(30).
           06 BirthDate.
             07 YYYY PIC 9(4).
             07 MM PIC 9(2).
             07 DD PIC 9(2).
                                       
      
       PROCEDURE DIVISION.
      
      *Problem with this line in codegen
      *    COMPUTE LK-RETENTION = FUNCTION MAX(Event::Periodicity(ALL)).
           COMPUTE LK-RETENTION = FUNCTION MAX(Periodicity OF Event
                                               (ALL)).
           DISPLAY LK-RETENTION.
      
      *    call 'subPgm' using AccountOwner::lastName
      *                     Group1::BirthDate
      *                     AccountOwner::BirthDate::YYYY.
           call 'subPgm' using lastName OF AccountOwner
                            BirthDate OF Group1
                            YYYY OF BirthDate OF AccountOwner.
      
      
       END PROGRAM Test-Name-Qualification.
      
