       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 group1-1.
             10 var1 PIC N.
             10 var2 PIC N.
       01 group2.
          05 group2-1.
             10 var3 PIC N.
             10 var4 PIC N.
             10 var5 PIC N.
          05 group2-2.
             10 var6 PIC N.
             10 var7 PIc N.
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
        "position": { "line": 16, "character": 26 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 1, "name": "group1", "ch": [
                    {
                        "vm": 1, "name": "group1-1", "ch": [
                            {
                                "vm": 0, "name": "var1"
                            }
                        ]
                    }
                ]
            },
            {
                "vm": 2, "name": "group2"
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
          05 group1-1.
             10 var1 PIC N.
             10 var2 PIC N.
       01 group2.
          05 group2-1.
             10 var3 PIC N.
             10 var4 PIC N.
             10 var5 PIC N.
          05 group2-2.
             10 var6 PIC N.
             10 var7 PIc N.
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY '    var1 <' FUNCTION DISPLAY-OF (var1) '>'
      D    DISPLAY 'group2'
      D    DISPLAY '  group2-1'
      D    DISPLAY '    var3 <' FUNCTION DISPLAY-OF (var3) '>'
      D    DISPLAY '    var4 <' FUNCTION DISPLAY-OF (var4) '>'
      D    DISPLAY '    var5 <' FUNCTION DISPLAY-OF (var5) '>'
      D    DISPLAY '  group2-2'
      D    DISPLAY '    var6 <' FUNCTION DISPLAY-OF (var6) '>'
      D    DISPLAY '    var7 <' FUNCTION DISPLAY-OF (var7) '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.