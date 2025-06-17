       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370 WITH DEBUGGING MODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 var1 PIC X.
          05 var2 PIC X.
       01 var3 PIC X.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM DEBUG-INFOS
           GOBACK
           .
       DEBUG-INFOS.
      *<DBG>Some previous generation by InsertVariableDisplay
      D    DISPLAY 'group1'
      D    DISPLAY '  var1 <' var1 '>'
      D    DISPLAY '  var2 <' var2 '>'
      *</DBG>
       
           .
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 23, "character": 7 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 0, "name": "var3"
            }
        ]
    }
]
-------------------------------------------------------------------------------------------------
refactoring.label=Debug instructions successfully generated.
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370 WITH DEBUGGING MODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 var1 PIC X.
          05 var2 PIC X.
       01 var3 PIC X.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM DEBUG-INFOS
           GOBACK
           .
       DEBUG-INFOS.
      *<DBG>Some previous generation by InsertVariableDisplay
      D    DISPLAY 'group1'
      D    DISPLAY '  var1 <' var1 '>'
      D    DISPLAY '  var2 <' var2 '>'
      *</DBG>
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'var3 <' var3 '>'
      *</DBG>

       
           .
       END PROGRAM TCOMFL06.