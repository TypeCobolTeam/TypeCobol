       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 .
          05 .
             10 var1 PIC X.
             10 FILLER PIC X(20).
             10 var2 PIC X.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.
------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.Refactor.InsertVariableDisplayRefactoringProcessor
------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 9, "character": 26 }
    },
    false,
    {
        "visitMode": 1, "index": 0, "subSelections": [
            {
                "visitMode": 1, "index": 0, "subSelections": [
                    {
                        "visitMode": 1, "index": 0, "subSelections": [
                            {
                                "visitMode": 0, "index": 1
                            }
                        ]
                    }
                ]
            }
        ]
    }
]
------------------------------------------------------------------------------------
refactoring.label=No modification.
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 .
          05 .
             10 var1 PIC X.
             10 FILLER PIC X(20).
             10 var2 PIC X.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.