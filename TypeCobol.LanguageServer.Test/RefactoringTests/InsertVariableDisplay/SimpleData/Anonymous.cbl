       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 select-some.
          05.
             10 PIC X(8).
             10 PIC X(5).
             10 PIC X(3).
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
        "position": { "line": 9, "character": 26 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 1, "name": "select-some", "ch": [
                    {
                        "vm": 1, "idx": 0, "ch": [
                            {
                                "vm": 0, "idx": 0
                            },
                            {
                                "vm": 0, "idx": 2
                            }
                        ]
                    }
                ]
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
       01 select-some.
          05.
             10 PIC X(8).
             10 PIC X(5).
             10 PIC X(3).
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY '    FILLER <' select-some (1:8) '>'
      D    DISPLAY '    FILLER <' select-some (14:3) '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.