       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOEMPTY.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 VAR-1 PIC X.

       PROCEDURE DIVISION.

           GOBACK
           .
       END PROGRAM TCOEMPTY.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 9, "character": 0 }
    },
    true,
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
       PROGRAM-ID. TCOEMPTY.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 VAR-1 PIC X.

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'VAR-1 <' VAR-1 '>'
      *</DBG>


           GOBACK
           .
       END PROGRAM TCOEMPTY.