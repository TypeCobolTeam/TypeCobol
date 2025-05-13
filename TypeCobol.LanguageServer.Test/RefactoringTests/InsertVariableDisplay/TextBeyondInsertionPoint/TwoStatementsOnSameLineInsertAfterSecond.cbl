       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC X(200).
       PROCEDURE DIVISION.
           DISPLAY "first" DISPLAY "second"
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 6, "character": 34 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 0, "name": "var1"
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
       01 var1 PIC X(200).
       PROCEDURE DIVISION.
           DISPLAY "first" DISPLAY "second"
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'var1 <' var1 '>'
      *</DBG>

       END PROGRAM TCOMFL06.