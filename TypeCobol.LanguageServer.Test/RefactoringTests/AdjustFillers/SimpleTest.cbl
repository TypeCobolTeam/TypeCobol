       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 target-zone PIC X(200).
       01 redef1 REDEFINES target-zone.
          05 var1 PIC X(100).
      * Adjust FILLER size here (20 -> 100)
          05 FILLER PIC X(20).
       01 redef2 REDEFINES target-zone.
          05 var2 PIC X(125).
      * Add here new FILLER, length 75
       01 redef3 REDEFINES target-zone.
          05 var3 PIC X(200).
      * Remove this FILLER entirely
          05 FILLER PIC X(25).
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.
---------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.AdjustFillers.AdjustFillersRefactoringProcessor
---------------------------------------------------------------------------------
[{"uri":"file:/test.expected.cbl"}]
---------------------------------------------------------------------------------
refactoring.label=Adjust FILLERs: 3 FILLER(s) modified
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 target-zone PIC X(200).
       01 redef1 REDEFINES target-zone.
          05 var1 PIC X(100).
      * Adjust FILLER size here (20 -> 100)
          05 FILLER PIC X(100).
       01 redef2 REDEFINES target-zone.
          05 var2 PIC X(125).
          05 FILLER PIC X(75).
      * Add here new FILLER, length 75
       01 redef3 REDEFINES target-zone.
          05 var3 PIC X(200).
      * Remove this FILLER entirely
                              
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.