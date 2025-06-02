       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC X.
       PROCEDURE DIVISION.
      * First line of comments
      * Second line of comments
      * Third line of comments
           DISPLAY "This statement is preceded by comments"
           GOBACK
           .
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 9, "character": 18 }
    },
    true,
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
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'var1 <' var1 '>'
      *</DBG>

      * First line of comments
      * Second line of comments
      * Third line of comments
           DISPLAY "This statement is preceded by comments"
           GOBACK
           .
       END PROGRAM TCOMFL06.