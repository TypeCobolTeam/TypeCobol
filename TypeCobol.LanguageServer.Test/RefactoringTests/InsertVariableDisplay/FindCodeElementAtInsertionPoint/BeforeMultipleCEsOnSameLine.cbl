       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VAR-1 PIC X.
       01 VAR-2 PIC X.
       PROCEDURE DIVISION.
           DISPLAY VAR-1 MOVE VAR-1 TO VAR-2 DISPLAY VAR-2
           GOBACK
           .
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 7, "character": 7 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 0, "name": "VAR-1"
            }
        ]
    }
]
-------------------------------------------------------------------------------------------------
refactoring.label=Debug instructions successfully generated.
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VAR-1 PIC X.
       01 VAR-2 PIC X.
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'VAR-1 <' VAR-1 '>'
      *</DBG>

           DISPLAY VAR-1 MOVE VAR-1 TO VAR-2 DISPLAY VAR-2
           GOBACK
           .
       END PROGRAM TCOMFL06.