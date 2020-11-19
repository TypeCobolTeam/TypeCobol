﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. DeclarativesTest.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      WITH DEBUGGING MODE
                      .
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ELEM occurs 12
                INDEXED BY IDX.
           05 NUM     pic 99.
           05 NBJ     pic 99.
           05 LIB    pic x(9).
      
       PROCEDURE DIVISION.
      
       DECLARATIVES.
      
       DEPT-HEAD-USE SECTION.
           USE FOR DEBUGGING ON ALL PROCEDURES.
       DEPT-HEAD-PROC.
           SET IDX to 1
           SEARCH ELEM
                  when NBJ(IDX) = 28
                  display NUM(IDX) "/" LIB(IDX).
           display "--------------------------------------".
      
       DEPT-HEAD-EXIT.
       EXIT.
      
       EMPL-FOOT-USE SECTION. USE FOR DEBUGGING ON ALL PROCEDURES.
       EMPL-FOOT-PROC.
       MOVE NUM TO NBJ.
       MOVE NBJ TO NUM.
       EMPL-FOOT-EXIT.
       EXIT.
      
       DEPT-FOOT-USE SECTION. USE FOR DEBUGGING ON ALL PROCEDURES.
       DEPT-FOOT-PROC.
       MOVE NUM TO NBJ.
       MOVE NBJ TO NUM.
      * SUPPRESS PRINTING.
      
       DEPT-FOOT-EXIT.
       EXIT.
      
       COMP-FOOT-USE SECTION. USE FOR DEBUGGING ON ALL PROCEDURES.
       COMP-FOOT-PROC.
       PERFORM COMP-FOOT-CALC
       VARYING IDX FROM +1 BY +1
       UNTIL IDX > +6.
       GO TO COMP-FOOT-EXIT.
      
       COMP-FOOT-CALC.
       MOVE NUM TO NBJ.
       MOVE NBJ TO NUM.
      
       COMP-FOOT-EXIT.
       EXIT.
      
       END DECLARATIVES.
       END PROGRAM DeclarativesTest.
      