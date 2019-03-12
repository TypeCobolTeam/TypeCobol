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
       01 ERIM04-LOT-SOC          PIC X(5).
       01 LOT-SOC                 PIC X(5).
       PROCEDURE DIVISION.
           EXEC SQL
             SELECT LOT_SOC              
             INTO :LOT-SOC             
             FROM TERILOT
             WHERE LOT_SOC             = :ERIM04-LOT-SOC
           END-EXEC  
           CONTINUE.
       END-DECLARE.

       declare procedure check public
          input mydate        TYPE Date
          output r1           PIC X(5)
         .
       data division.
       working-storage section.
       LINKAGE SECTION.
       01 ERIM04-LOT-SOC          PIC X(5).
       01 LOT-SOC                 PIC X(5).
       PROCEDURE DIVISION.
           DISPLAY "P1"
           EXEC SQL
             SELECT LOT_SOC              
             INTO :LOT-SOC             
             FROM TERILOT
             WHERE LOT_SOC             = :ERIM04-LOT-SOC
           END-EXEC  
           CONTINUE.
       END-DECLARE.
       
       declare procedure checkName private
          input myname        PIC X(15)
         .
       data division.
       working-storage section.
       01 var1 type Date.
       linkage section.
       PROCEDURE DIVISION.
           Call check input var1
           .
       END-DECLARE.
           
       INIT-LIBRARY.
            EXIT.


       END PROGRAM PGM1.

