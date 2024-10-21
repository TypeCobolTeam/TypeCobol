       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 target-zone PIC X(200).
       01 redef1 REDEFINES target-zone.
          05 var1 PIC X(100).
      * Ajuster ici la taille (20 -> 100)
          05 FILLER PIC X(20).
       01 redef2 REDEFINES target-zone.
          05 var2 PIC X(125).
      * Ajouter ici un FILLER de taille 75
       01 redef3 REDEFINES target-zone.
          05 var3 PIC X(200).
      * Supprimer ce FILLER complètement
          05 FILLER PIC X(25).
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.
---------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.Refactor.AdjustFillerRefactoringProcessor
---------------------------------------------------------------------------
[{"uri":"file:/test.expected.cbl"}]
---------------------------------------------------------------------------
refactoring.label=Adjust FILLERs: 3 FILLER(s) modified
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 target-zone PIC X(200).
       01 redef1 REDEFINES target-zone.
          05 var1 PIC X(100).
      * Ajuster ici la taille (20 -> 100)
          05 FILLER PIC X(100).
       01 redef2 REDEFINES target-zone.
          05 var2 PIC X(125).
          05 FILLER PIC X(75).
      * Ajouter ici un FILLER de taille 75
       01 redef3 REDEFINES target-zone.
          05 var3 PIC X(200).
      * Supprimer ce FILLER complètement
                              
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.