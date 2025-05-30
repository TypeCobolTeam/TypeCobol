       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC X.
       PROCEDURE DIVISION.
           DISPLAY "This statement is followed by a comment"
      * Line of comments
           GOBACK
           .
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 6, "character": 18 }
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
       01 var1 PIC X.
       PROCEDURE DIVISION.
           DISPLAY "This statement is followed by a comment"
      * Line of comments
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'var1 <' var1 '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.