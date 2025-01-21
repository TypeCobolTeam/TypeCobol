       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 group1-1.
             10 var1 PIC X.
             10 var2 PIC X.
       01 group2.
          05 group2-1.
             10 var3 PIC X.
             10 var4 PIC X.
             10 var5 PIC X.
          05 group2-2.
             10 var6 PIC X.
             10 var7 PIc X.
       01 group3.
          05 group3-1.
             10 var8 PIC X.
          05 group3-2.
             10 var9 PIC X.
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
        "position": { "line": 21, "character": 26 }
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
            },
            {
                "vm": 0, "name": "group3", "ch": [
                    {
                        "vm": 0, "name": "group3-1"
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
       01 group1.
          05 group1-1.
             10 var1 PIC X.
             10 var2 PIC X.
       01 group2.
          05 group2-1.
             10 var3 PIC X.
             10 var4 PIC X.
             10 var5 PIC X.
          05 group2-2.
             10 var6 PIC X.
             10 var7 PIc X.
       01 group3.
          05 group3-1.
             10 var8 PIC X.
          05 group3-2.
             10 var9 PIC X.
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY '    var1 <' var1 '>'
      D    DISPLAY 'group2'
      D    DISPLAY '  group2-1'
      D    DISPLAY '    var3 <' var3 '>'
      D    DISPLAY '    var4 <' var4 '>'
      D    DISPLAY '    var5 <' var5 '>'
      D    DISPLAY '  group2-2'
      D    DISPLAY '    var6 <' var6 '>'
      D    DISPLAY '    var7 <' var7 '>'
      D    DISPLAY 'group3'
      D    DISPLAY '  group3-1 <' group3-1 '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.