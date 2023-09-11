       IDENTIFICATION DIVISION.
       PROGRAM-ID.   Test-Name-Qualification.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01 _EVENT_ TYPEDEF STRICT.
          05 IdNbr PIC X(05).
          05 Periodicity PIC 9(03).
      
       01 EventList.
          05 Event TYPE _EVENT_ OCCURS 200.
             
       01 LK-RETENTION PIC 9(4).

       01 Person TYPEDEF STRICT.
           05 LastName pic X(30).
           05 FirstName pic X(30).
           05 BirthDate type date.
      
       01 Group1.
           05 AccountOwner TYPE Person.

       PROCEDURE DIVISION.

      *Problem with this line in codegen
           COMPUTE LK-RETENTION = FUNCTION MAX(Event::Periodicity(ALL)).
           DISPLAY LK-RETENTION.

           call 'subPgm' using AccountOwner::lastName
                            Group1::BirthDate
                            AccountOwner::BirthDate::YYYY.

      
       END PROGRAM Test-Name-Qualification.
      