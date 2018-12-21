       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.

       DATA DIVISION.
       Working-STORAGE SECTION.

       01  mydate1     TYPE Date.
       01  myname1      PIC X(15).

       PROCEDURE DIVISiON.

        declare procedure check public
          input mydate        TYPE Date
         .
       data division.
       working-storage section.
       LINKAGE SECTION.
       01 ERIM04-LOT-SOC                 PIC X(5).
       01 LOT-SOC                 PIC X(5).
       PROCEDURE DIVISION.
           EXEC SQL
             SELECT
               LOT_SOC              

      
             INTO
               :LOT-SOC             
      
             FROM TERILOT
      
             WHERE LOT_SOC             = :ERIM04-LOT-SOC

           END-EXEC  
           CONTINUE.
       END-DECLARE.

       declare procedure checkName public
          input myname        PIC X(15)
         .
       data division.
       working-storage section.
       linkage section.
       PROCEDURE DIVISION.
           Call PersonService::GetPersonByName input myname
           .
       END-DECLARE.

       INIT-LIBRARY.
            EXIT.
       
       TRAITEMENT.
      *OK  call check of PGM1
      *   call check input mydate1
      *    .

      *PersonService contains public procedure
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PersonService.
       
       
       PROCEDURE DIVISION.       
       declare procedure GetPersonById private
          input  personId  type date.
          
       DATA DIVISION.
       LINKAGE SECTION.
       01 ERIM04-LOT-SOC                 PIC X(5).
       01 LOT-SOC                 PIC X(5).
       PROCEDURE DIVISION.
           EXEC SQL
             SELECT
               LOT_SOC              

      
             INTO
               :LOT-SOC             
      
             FROM TERILOT
      
             WHERE LOT_SOC             = :ERIM04-LOT-SOC

           END-EXEC  
           CONTINUE.
       END-DECLARE.
       
       declare procedure GetPersonByName public
          input  name  pic x(15).
         PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.
       
       INIT-LIBRARY.
            EXIT.
       
       END PROGRAM PersonService.


       END PROGRAM PGM1.

