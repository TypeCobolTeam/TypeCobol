       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 var1 PIC X.
          05 var2 PIC X.
          05 var3 PIC X.
       PROCEDURE DIVISION.
           CALL "somepgm" USING var1
                                var2
                                var3
           GOBACK
           .
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 10, "character": 36 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 2, "name": "group1"
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
       01 group1.
          05 var1 PIC X.
          05 var2 PIC X.
          05 var3 PIC X.
       PROCEDURE DIVISION.
           CALL "somepgm" USING var1
                                var2
                                var3
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'group1'
      D    DISPLAY '  var1 <' var1 '>'
      D    DISPLAY '  var2 <' var2 '>'
      D    DISPLAY '  var3 <' var3 '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.