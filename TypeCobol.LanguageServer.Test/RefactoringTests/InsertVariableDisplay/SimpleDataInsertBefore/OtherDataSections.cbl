       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var-working PIC X(200).
       LOCAL-STORAGE SECTION.
       01 var-local PIC X(200).
       LINKAGE SECTION.
       01 var-linkage PIC X(200).
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 10, "character": 11 }
    },
    true,
    {
        "vm": 2, "idx": 0
    },
    {
        "vm": 2, "idx": 1
    },
    {
        "vm": 2, "idx": 2
    }
]
-------------------------------------------------------------------------------------------------
refactoring.label=Debug instructions successfully generated.
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var-working PIC X(200).
       LOCAL-STORAGE SECTION.
       01 var-local PIC X(200).
       LINKAGE SECTION.
       01 var-linkage PIC X(200).
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'var-working <' var-working '>'
      D    DISPLAY 'var-local <' var-local '>'
      D    DISPLAY 'var-linkage <' var-linkage '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.