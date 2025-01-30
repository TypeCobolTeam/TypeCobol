       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 group1-1.
             10 var1 PIC X.
             10 FILLER PIC X(20).
             10 var2 PIC X.
       01 group2.
          05 group2-1.
             10 var3 PIC X.
             10      PIC X.
             10 var5 PIC X.
       01 group3.
          05 var6 PIC X(10).
          05 .
             10 var7 PIC X(5).
             10 FILLER PIC X(20).
       01 .
          05 .
             10 var8 PIC X.
             10 FILLER PIC X(20).
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
        "position": { "line": 25, "character": 11 }
    },
    true,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 1, "name": "group1", "ch": [
                    {
                        "vm": 1, "name": "group1-1", "ch": [
                            {
                                "vm": 0, "idx": 1
                            }
                        ]
                    }
                ]
            },
            {
                "vm": 1, "name": "group2", "ch": [
                    {
                        "vm": 1, "name": "group2-1", "ch": [
                            {
                                "vm": 0, "idx": 1
                            }
                        ]
                    }
                ]
            },
            {
                "vm": 1, "name": "group3", "ch": [
                    {
                        "vm": 1, "idx": 1, "ch": [
                            {
                                "vm": 0, "idx": 1
                            }
                        ]
                    }
                ]
            },
            {
                "vm": 1, "idx": 3, "ch": [
                    {
                        "vm": 1, "idx": 0, "ch": [
                            {
                                "vm": 0, "idx": 1
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
       01 group1.
          05 group1-1.
             10 var1 PIC X.
             10 FILLER PIC X(20).
             10 var2 PIC X.
       01 group2.
          05 group2-1.
             10 var3 PIC X.
             10      PIC X.
             10 var5 PIC X.
       01 group3.
          05 var6 PIC X(10).
          05 .
             10 var7 PIC X(5).
             10 FILLER PIC X(20).
       01 .
          05 .
             10 var8 PIC X.
             10 FILLER PIC X(20).
             10 var9 PIC X.
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY '    FILLER (2:20) <' group1-1 (2:20) '>'
      D    DISPLAY '    FILLER (2:1) <' group2-1 (2:1) '>'
      D    DISPLAY '    FILLER <' group3 (16:20) '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.