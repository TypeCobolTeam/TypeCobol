       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOZERRCP.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 VAR-1 PIC X.
      * Error: VAR 2 instead of VAR-2
       01 VAR 2 PIC X.

       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOZERRCP.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 10, "character": 26 }
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
System.InvalidOperationException
Unable to locate DISPLAY insertion location.
