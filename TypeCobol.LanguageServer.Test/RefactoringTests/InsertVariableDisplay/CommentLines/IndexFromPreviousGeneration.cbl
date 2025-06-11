       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370 WITH DEBUGGING MODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 tab1 OCCURS 10.
             10 var1 PIC X.
      *<DBG> Another index from previous generation
      D77 Idx-a1b2c3d4-1 PIC 9(4) COMP-5.
      *</DBG>
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
        "position": { "line": 14, "character": 26 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 1, "name": "group1", "ch": [
                    {
                        "vm": 1, "name": "tab1", "ch": [
                            {
                                "vm": 0, "name": "var1"
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
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370 WITH DEBUGGING MODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 tab1 OCCURS 10.
             10 var1 PIC X.
      *<DBG> Another index from previous generation
      D77 Idx-a1b2c3d4-1 PIC 9(4) COMP-5.
      *</DBG>
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-d4df4249-1 PIC 9(4) COMP-5.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    PERFORM VARYING Idx-d4df4249-1 FROM 1 BY 1 UNTIL
      D    Idx-d4df4249-1 > 10
      D      DISPLAY '    var1 (' Idx-d4df4249-1 ') <' var1
      D      (Idx-d4df4249-1) '>'
      D      DISPLAY '    ---------------------------------------------'
      D              '-------------------------------------------------'
      D              '----------------------'
      D    END-PERFORM
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.